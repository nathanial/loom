/-
  Loom.ActionM - Monadic action interface with implicit context threading

  ActionM is a StateT-based monad that eliminates explicit ctx parameter passing.
  Use `.toAction` to convert back to the traditional `Action` type for routing.
-/
import Loom.Controller
import Loom.Transaction
import Loom.Audit

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

/-! ## Transaction Monad Integration -/

/-- Run a TxM transaction builder and commit the result.
    Returns the computation result on success, or an error. -/
def runTx (m : Ledger.TxM α) : ActionM (Except Ledger.TxError α) := do
  let ctx ← get
  match ctx.database with
  | none => pure (.error (.custom "No database connection"))
  | some db =>
    let (result, ops) := Ledger.TxM.build m db
    if ops.isEmpty then
      pure (.ok result)
    else
      match ← transact ops.toList with
      | .ok () => pure (.ok result)
      | .error e => pure (.error e)

/-- Run a TxM transaction builder and commit, throwing on error -/
def runTx! (m : Ledger.TxM α) : ActionM α := do
  match ← runTx m with
  | .ok result => pure result
  | .error e => throw (IO.userError s!"Transaction failed: {e}")

/-- Allocate a new entity ID and run a transaction builder with it.
    Returns the allocated ID and computation result on success. -/
def withNewEntity (f : Ledger.EntityId → Ledger.TxM α) : ActionM (Except Ledger.TxError (Ledger.EntityId × α)) := do
  match ← allocEntityId with
  | none => pure (.error (.custom "No database connection"))
  | some eid =>
    match ← runTx (f eid) with
    | .ok result => pure (.ok (eid, result))
    | .error e => pure (.error e)

/-- Allocate a new entity ID and run a transaction builder, throwing on error -/
def withNewEntity! (f : Ledger.EntityId → Ledger.TxM α) : ActionM (Ledger.EntityId × α) := do
  match ← withNewEntity f with
  | .ok pair => pure pair
  | .error e => throw (IO.userError s!"Transaction failed: {e}")

/-- Allocate multiple entity IDs and run a transaction builder with them -/
def withNewEntities (n : Nat) (f : List Ledger.EntityId → Ledger.TxM α)
    : ActionM (Except Ledger.TxError (List Ledger.EntityId × α)) := do
  let mut eids : List Ledger.EntityId := []
  for _ in [:n] do
    match ← allocEntityId with
    | none => return .error (.custom "No database connection")
    | some eid => eids := eids ++ [eid]
  match ← runTx (f eids) with
  | .ok result => pure (.ok (eids, result))
  | .error e => pure (.error e)

/-- Allocate multiple entity IDs and run a transaction builder, throwing on error -/
def withNewEntities! (n : Nat) (f : List Ledger.EntityId → Ledger.TxM α)
    : ActionM (List Ledger.EntityId × α) := do
  match ← withNewEntities n f with
  | .ok pair => pure pair
  | .error e => throw (IO.userError s!"Transaction failed: {e}")

/-! ## Audit Transaction Monad Integration -/

/-- Run an AuditTxM transaction builder, commit, and log all audit entries.
    Returns the computation result on success, or an error. -/
def runAuditTx (m : AuditTxM α) : ActionM (Except Ledger.TxError α) := do
  let ctx ← get
  match ctx.database with
  | none => pure (.error (.custom "No database connection"))
  | some db =>
    let (result, ops, auditEntries) := AuditTxM.build m db
    if ops.isEmpty then
      -- Log audit entries even if no db ops
      let ctx ← get
      for entry in auditEntries do
        logAudit ctx entry.op entry.entity entry.entityId entry.details
      pure (.ok result)
    else
      match ← transact ops.toList with
      | .ok () =>
        -- Log audit entries after successful commit
        let ctx ← get
        for entry in auditEntries do
          logAudit ctx entry.op entry.entity entry.entityId entry.details
        pure (.ok result)
      | .error e => pure (.error e)

/-- Run an AuditTxM transaction builder and commit, throwing on error -/
def runAuditTx! (m : AuditTxM α) : ActionM α := do
  match ← runAuditTx m with
  | .ok result => pure result
  | .error e => throw (IO.userError s!"Transaction failed: {e}")

/-- Allocate a new entity ID and run an AuditTxM transaction builder with it.
    Returns the allocated ID and computation result on success. -/
def withNewEntityAudit (f : Ledger.EntityId → AuditTxM α)
    : ActionM (Except Ledger.TxError (Ledger.EntityId × α)) := do
  match ← allocEntityId with
  | none => pure (.error (.custom "No database connection"))
  | some eid =>
    match ← runAuditTx (f eid) with
    | .ok result => pure (.ok (eid, result))
    | .error e => pure (.error e)

/-- Allocate a new entity ID and run an AuditTxM transaction builder, throwing on error -/
def withNewEntityAudit! (f : Ledger.EntityId → AuditTxM α) : ActionM (Ledger.EntityId × α) := do
  match ← withNewEntityAudit f with
  | .ok pair => pure pair
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

/-- Create a 303 See Other redirect (forces GET on redirect, use after POST/PUT/DELETE) -/
def seeOther (location : String) : ActionM Herald.Core.Response :=
  pure (Citadel.Response.seeOther location)

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
