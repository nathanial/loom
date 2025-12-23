/-
  Loom.Controller - Request context and action helpers
-/
import Citadel
import Scribe
import Ledger
import Loom.Cookie
import Loom.Session
import Loom.Flash
import Loom.Form
import Loom.Database

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

/-- Update session in context -/
def withSession (ctx : Context) (f : Session → Session) : Context :=
  { ctx with session := f ctx.session }

/-- Update flash in context -/
def withFlash (ctx : Context) (f : Flash → Flash) : Context :=
  { ctx with flash := f ctx.flash }

/-- Update database connection in context -/
def withDb (ctx : Context) (conn : Ledger.Connection) : Context :=
  { ctx with db := some conn }

-- ============================================================================
-- Database helpers
-- ============================================================================

/-- Get the current database snapshot (if available). -/
def database (ctx : Context) : Option Ledger.Db :=
  ctx.db.map (·.db)

/-- Execute a query against the database.
    Returns None if no database is configured. -/
def query (ctx : Context) (q : Ledger.Query) : Option Ledger.Query.QueryResult :=
  ctx.db.map fun conn => Ledger.Query.execute q conn.db

/-- Execute a transaction against the database.
    Returns an updated context with the new connection state, or an error. -/
def transact (ctx : Context) (tx : Ledger.Transaction) : IO (Except Ledger.TxError Context) := do
  match ctx.db with
  | none => pure (Except.error (.custom "No database connection"))
  | some conn =>
    match conn.transact tx with
    | Except.ok (newConn, _report) =>
      pure (Except.ok { ctx with db := some newConn })
    | Except.error e => pure (Except.error e)

/-- Execute a transaction and get both the updated context and the report. -/
def transactWithReport (ctx : Context) (tx : Ledger.Transaction)
    : IO (Except Ledger.TxError (Context × Ledger.TxReport)) := do
  match ctx.db with
  | none => pure (Except.error (.custom "No database connection"))
  | some conn =>
    match conn.transact tx with
    | Except.ok (newConn, report) =>
      pure (Except.ok ({ ctx with db := some newConn }, report))
    | Except.error e => pure (Except.error e)

/-- Check if a database connection is available. -/
def hasDatabase (ctx : Context) : Bool :=
  ctx.db.isSome

end Context

/-- Action type: takes context, returns response -/
abbrev Action := Context → IO Herald.Core.Response

namespace Action

/-- Create a text response -/
def text (content : String) : IO Herald.Core.Response :=
  pure (Citadel.Response.ok content)

/-- Create an HTML response -/
def html (content : String) : IO Herald.Core.Response :=
  pure (Citadel.Response.html content)

/-- Create a JSON response -/
def json (content : String) : IO Herald.Core.Response :=
  pure (Citadel.Response.json content)

/-- Create a redirect response -/
def redirect (location : String) (permanent : Bool := false) : IO Herald.Core.Response :=
  pure (Citadel.Response.redirect location permanent)

/-- Create a not found response -/
def notFound (message : String := "Not Found") : IO Herald.Core.Response :=
  pure (Citadel.Response.notFound message)

/-- Create a bad request response -/
def badRequest (message : String := "Bad Request") : IO Herald.Core.Response :=
  pure (Citadel.Response.badRequest message)

/-- Create a forbidden response -/
def forbidden (message : String := "Forbidden") : IO Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus (Herald.Core.StatusCode.mk 403)
    |>.withText message
    |>.build)

/-- Create an unauthorized response -/
def unauthorized (message : String := "Unauthorized") : IO Herald.Core.Response :=
  pure (Citadel.ResponseBuilder.withStatus (Herald.Core.StatusCode.mk 401)
    |>.withText message
    |>.build)

end Action

/-- CSRF hidden field for forms -/
def csrfField (token : String) : Scribe.HtmlM Unit :=
  Scribe.input [Scribe.type_ "hidden", Scribe.name_ "_csrf", Scribe.value_ token]

/-- Generate a CSRF meta tag for AJAX requests -/
def csrfMetaTag (token : String) : Scribe.HtmlM Unit := do
  Scribe.meta_ [Scribe.name_ "csrf-token", Scribe.content_ token]

end Loom
