/-
  Loom.Controller - Request context and action helpers
-/
import Citadel
import Scribe
import Ledger
import Chronicle
import Loom.Cookie
import Loom.Session
import Loom.Flash
import Loom.Form
import Loom.Multipart
import Loom.Database
import Loom.Stencil.Manager

namespace Loom

/-- Application configuration -/
structure AppConfig where
  /-- Secret key for signing cookies and CSRF tokens -/
  secretKey : ByteArray
  /-- Name of the session cookie -/
  sessionCookieName : String := "loom_session"
  /-- Name of the CSRF form field -/
  csrfFieldName : String := "_csrf"
  /-- Path to static files (None to disable) -/
  staticPath : Option String := some "public"
  /-- Enable CSRF protection -/
  csrfEnabled : Bool := true
  /-- Database configuration (None to disable) -/
  dbConfig : Option Database.DbConfig := none
  /-- Development mode - disables static file caching -/
  devMode : Bool := true

/-- Request context available to actions -/
structure Context where
  /-- The underlying HTTP request -/
  request : Citadel.ServerRequest
  /-- Session data -/
  session : Session
  /-- Flash messages -/
  flash : Flash
  /-- Merged parameters (path + query + form) -/
  params : Params
  /-- Application config -/
  config : AppConfig
  /-- CSRF token for this request -/
  csrfToken : String
  /-- Database connection (if configured) -/
  db : Option Ledger.Connection := none
  /-- Persistent database connection (if configured with persistence) -/
  persistentDb : Option Ledger.Persist.PersistentConnection := none
  /-- Logger for application-level logging -/
  logger : Option Chronicle.MultiLogger := none
  /-- Parsed multipart data (for file uploads) -/
  multipartData : Option MultipartData := none
  /-- Stencil template manager (if configured) -/
  stencilManager : Option (IO.Ref Stencil.Manager) := none

namespace Context

/-- Get the HTTP method -/
def method (ctx : Context) : Herald.Core.Method :=
  ctx.request.method

/-- Get the request path -/
def path (ctx : Context) : String :=
  Form.pathWithoutQuery ctx.request.path

/-- Get a parameter by name -/
def param (ctx : Context) (name : String) : Option String :=
  ctx.params.get name

/-- Get a parameter with default -/
def paramD (ctx : Context) (name : String) (default : String) : String :=
  ctx.params.getD name default

/-- Get a header value -/
def header (ctx : Context) (name : String) : Option String :=
  ctx.request.header name

/-- Get the request body as a string -/
def body (ctx : Context) : String :=
  ctx.request.bodyString

/-- Check if request is AJAX/XHR -/
def isXhr (ctx : Context) : Bool :=
  ctx.header "X-Requested-With" == some "XMLHttpRequest"

/-- Check if request accepts JSON -/
def acceptsJson (ctx : Context) : Bool :=
  match ctx.header "Accept" with
  | some accept => (accept.splitOn "application/json").length > 1
  | none => false

/-- Get the content type of the request -/
def contentType (ctx : Context) : Option String :=
  ctx.header "Content-Type"

/-- Check if request is a multipart form submission -/
def isMultipart (ctx : Context) : Bool :=
  ctx.multipartData.isSome

/-- Get an uploaded file by field name -/
def file (ctx : Context) (name : String) : Option MultipartPart :=
  ctx.multipartData.bind fun mpd => mpd.getFile name

/-- Get all uploaded files with a given field name -/
def filesByName (ctx : Context) (name : String) : List MultipartPart :=
  ctx.multipartData.map (·.getFilesByName name) |>.getD []

/-- Get all uploaded files -/
def files (ctx : Context) : List MultipartPart :=
  ctx.multipartData.map (·.getFiles) |>.getD []

/-- Check if request has file uploads -/
def hasFiles (ctx : Context) : Bool :=
  match ctx.multipartData with
  | some mpd => !mpd.getFiles.isEmpty
  | none => false

/-- Update session in context -/
def withSession (ctx : Context) (f : Session → Session) : Context :=
  { ctx with session := f ctx.session }

/-- Update flash in context -/
def withFlash (ctx : Context) (f : Flash → Flash) : Context :=
  { ctx with flash := f ctx.flash }

/-- Update database connection in context -/
def withDb (ctx : Context) (conn : Ledger.Connection) : Context :=
  { ctx with db := some conn }

/-- Update persistent database connection in context -/
def withPersistentDb (ctx : Context) (conn : Ledger.Persist.PersistentConnection) : Context :=
  { ctx with persistentDb := some conn }

/-- Update logger in context -/
def withLogger (ctx : Context) (logger : Chronicle.MultiLogger) : Context :=
  { ctx with logger := some logger }

-- ============================================================================
-- Database helpers
-- ============================================================================

/-- Get the current database snapshot (if available).
    Prefers persistent connection if available. -/
def database (ctx : Context) : Option Ledger.Db :=
  match ctx.persistentDb with
  | some pc => some pc.db
  | none => ctx.db.map (·.db)

/-- Execute a query against the database.
    Returns None if no database is configured.
    Prefers persistent connection if available. -/
def query (ctx : Context) (q : Ledger.Query) : Option Ledger.Query.QueryResult :=
  match ctx.persistentDb with
  | some pc => some (Ledger.Query.execute q pc.db)
  | none => ctx.db.map fun conn => Ledger.Query.execute q conn.db

/-- Execute a transaction against the database.
    Returns an updated context with the new connection state, or an error.
    Uses persistent connection if available (auto-persists to JSONL). -/
def transact (ctx : Context) (tx : Ledger.Transaction) : IO (Except Ledger.TxError Context) := do
  match ctx.persistentDb with
  | some pc =>
    match ← pc.transact tx with
    | Except.ok (newPc, _report) =>
      pure (Except.ok { ctx with persistentDb := some newPc })
    | Except.error e => pure (Except.error e)
  | none =>
    match ctx.db with
    | none => pure (Except.error (.custom "No database connection"))
    | some conn =>
      match conn.transact tx with
      | Except.ok (newConn, _report) =>
        pure (Except.ok { ctx with db := some newConn })
      | Except.error e => pure (Except.error e)

/-- Execute a transaction and get both the updated context and the report.
    Uses persistent connection if available (auto-persists to JSONL). -/
def transactWithReport (ctx : Context) (tx : Ledger.Transaction)
    : IO (Except Ledger.TxError (Context × Ledger.TxReport)) := do
  match ctx.persistentDb with
  | some pc =>
    match ← pc.transact tx with
    | Except.ok (newPc, report) =>
      pure (Except.ok ({ ctx with persistentDb := some newPc }, report))
    | Except.error e => pure (Except.error e)
  | none =>
    match ctx.db with
    | none => pure (Except.error (.custom "No database connection"))
    | some conn =>
      match conn.transact tx with
      | Except.ok (newConn, report) =>
        pure (Except.ok ({ ctx with db := some newConn }, report))
      | Except.error e => pure (Except.error e)

/-- Check if a database connection is available.
    Returns true if either persistent or in-memory connection is set. -/
def hasDatabase (ctx : Context) : Bool :=
  ctx.persistentDb.isSome || ctx.db.isSome

/-- Allocate a new entity ID from the database.
    Returns the new ID and updated context, or None if no database. -/
def allocEntityId (ctx : Context) : Option (Ledger.EntityId × Context) :=
  match ctx.persistentDb with
  | some pc =>
    let (eid, pc') := pc.allocEntityId
    some (eid, { ctx with persistentDb := some pc' })
  | none =>
    match ctx.db with
    | some conn =>
      let (eid, conn') := conn.allocEntityId
      some (eid, { ctx with db := some conn' })
    | none => none

end Context

/-- Action result: response paired with modified context -/
abbrev ActionResult := Herald.Core.Response × Context

/-- Action type: takes context, returns response with modified context -/
abbrev Action := Context → IO ActionResult

namespace Action

/-- Create a text response preserving context -/
def text (content : String) (ctx : Context) : IO ActionResult :=
  pure (Citadel.Response.ok content, ctx)

/-- Create an HTML response preserving context -/
def html (content : String) (ctx : Context) : IO ActionResult :=
  pure (Citadel.Response.html content, ctx)

/-- Create a JSON response preserving context -/
def json (content : String) (ctx : Context) : IO ActionResult :=
  pure (Citadel.Response.json content, ctx)

/-- Create a redirect response preserving context -/
def redirect (location : String) (ctx : Context) (permanent : Bool := false) : IO ActionResult :=
  pure (Citadel.Response.redirect location permanent, ctx)

/-- Create a not found response preserving context -/
def notFound (ctx : Context) (message : String := "Not Found") : IO ActionResult :=
  pure (Citadel.Response.notFound message, ctx)

/-- Create a bad request response preserving context -/
def badRequest (ctx : Context) (message : String := "Bad Request") : IO ActionResult :=
  pure (Citadel.Response.badRequest message, ctx)

/-- Create a forbidden response preserving context -/
def forbidden (ctx : Context) (message : String := "Forbidden") : IO ActionResult :=
  pure (Citadel.ResponseBuilder.withStatus (Herald.Core.StatusCode.mk 403)
    |>.withText message
    |>.build, ctx)

/-- Create an unauthorized response preserving context -/
def unauthorized (ctx : Context) (message : String := "Unauthorized") : IO ActionResult :=
  pure (Citadel.ResponseBuilder.withStatus (Herald.Core.StatusCode.mk 401)
    |>.withText message
    |>.build, ctx)

end Action

/-- CSRF hidden field for forms -/
def csrfField (token : String) : Scribe.HtmlM Unit :=
  Scribe.input [Scribe.type_ "hidden", Scribe.name_ "_csrf", Scribe.value_ token]

/-- Generate a CSRF meta tag for AJAX requests -/
def csrfMetaTag (token : String) : Scribe.HtmlM Unit :=
  Scribe.meta_ [Scribe.name_ "csrf-token", Scribe.content_ token]

end Loom
