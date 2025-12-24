/-
  Loom.ActionM - Monadic action interface with implicit context threading

  ActionM is a StateT-based monad that eliminates explicit ctx parameter passing.
  Use `.toAction` to convert back to the traditional `Action` type for routing.
-/
import Loom.Controller

namespace Loom

/-- Action monad with implicit context threading via StateT -/
abbrev ActionM := StateT Context IO

namespace ActionM

/-! ## Context Access -/

/-- Get the current context -/
def getCtx : ActionM Context := get

/-- Modify the context -/
def modifyCtx (f : Context → Context) : ActionM Unit := modify f

/-! ## Request Data -/

/-- Get a request parameter by name -/
def param (name : String) : ActionM (Option String) := do
  let ctx ← get
  pure (ctx.params.get name)

/-- Get a request parameter with default -/
def paramD (name : String) (default : String) : ActionM String := do
  let ctx ← get
  pure (ctx.params.getD name default)

/-- Get a request header -/
def header (name : String) : ActionM (Option String) := do
  let ctx ← get
  pure (ctx.request.header name)

/-- Get the request body -/
def body : ActionM String := do
  let ctx ← get
  pure ctx.request.bodyString

/-- Get the request path -/
def path : ActionM String := do
  let ctx ← get
  pure ctx.path

/-- Get the HTTP method -/
def method : ActionM Herald.Core.Method := do
  let ctx ← get
  pure ctx.method

/-- Get the CSRF token -/
def csrfToken : ActionM String := do
  let ctx ← get
  pure ctx.csrfToken

/-! ## Session -/

/-- Get a session value -/
def sessionGet (key : String) : ActionM (Option String) := do
  let ctx ← get
  pure (ctx.session.get key)

/-- Set a session value -/
def sessionSet (key value : String) : ActionM Unit :=
  modify fun ctx => ctx.withSession fun s => s.set key value

/-- Check if session has a key -/
def sessionHas (key : String) : ActionM Bool := do
  let ctx ← get
  pure (ctx.session.has key)

/-- Clear the session -/
def sessionClear : ActionM Unit :=
  modify fun ctx => ctx.withSession fun s => s.clear

/-! ## Flash Messages -/

/-- Set a flash message -/
def flash (key value : String) : ActionM Unit :=
  modify fun ctx => ctx.withFlash fun f => f.set key value

/-- Set an error flash message -/
def flashError (msg : String) : ActionM Unit := flash "error" msg

/-- Set a success flash message -/
def flashSuccess (msg : String) : ActionM Unit := flash "success" msg

/-- Set an info flash message -/
def flashInfo (msg : String) : ActionM Unit := flash "info" msg

/-- Get a flash message -/
def flashGet (key : String) : ActionM (Option String) := do
  let ctx ← get
  pure (ctx.flash.get key)

/-! ## Database -/

/-- Get the current database snapshot -/
def database : ActionM (Option Ledger.Db) := do
  let ctx ← get
  pure ctx.database

/-- Check if database is available -/
def hasDatabase : ActionM Bool := do
  let ctx ← get
  pure ctx.hasDatabase

/-- Allocate a new entity ID. Updates context with new connection state. -/
def allocEntityId : ActionM (Option Ledger.EntityId) := do
  let ctx ← get
  match ctx.allocEntityId with
  | some (eid, ctx') => set ctx'; pure (some eid)
  | none => pure none

/-- Execute a database transaction. Updates context with new connection state. -/
def transact (tx : Ledger.Transaction) : ActionM (Except Ledger.TxError Unit) := do
  let ctx ← get
  match ← ctx.transact tx with
  | .ok ctx' => set ctx'; pure (.ok ())
  | .error e => pure (.error e)

/-- Execute a transaction, throwing on error -/
def transact! (tx : Ledger.Transaction) : ActionM Unit := do
  match ← transact tx with
  | .ok () => pure ()
  | .error e => throw (IO.userError s!"Transaction failed: {e}")

/-! ## Response Builders -/

/-- Create an HTML response -/
def html (content : String) : ActionM Herald.Core.Response :=
  pure (Citadel.Response.html content)

/-- Create a text response -/
def text (content : String) : ActionM Herald.Core.Response :=
  pure (Citadel.Response.ok content)

/-- Create a JSON response -/
def json (content : String) : ActionM Herald.Core.Response :=
  pure (Citadel.Response.json content)

/-- Create a redirect response -/
def redirect (location : String) (permanent : Bool := false) : ActionM Herald.Core.Response :=
  pure (Citadel.Response.redirect location permanent)

/-- Create a not found response -/
def notFound (message : String := "Not Found") : ActionM Herald.Core.Response :=
  pure (Citadel.Response.notFound message)

/-- Create a bad request response -/
def badRequest (message : String := "Bad Request") : ActionM Herald.Core.Response :=
  pure (Citadel.Response.badRequest message)

/-! ## Conversion -/

/-- Convert an ActionM to an Action for use with the router -/
def toAction (m : ActionM Herald.Core.Response) : Action := fun ctx => do
  let (resp, ctx') ← m.run ctx
  pure (resp, ctx')

end ActionM

/-! ## ToAction typeclass for polymorphic route registration -/

/-- Typeclass for types that can be converted to an Action -/
class ToAction (α : Type) where
  toAction : α → Action

instance : ToAction Action where
  toAction a := a

instance : ToAction (ActionM Herald.Core.Response) where
  toAction m := m.toAction

end Loom
