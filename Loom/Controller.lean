/-
  Loom.Controller - Request context and action helpers
-/
import Citadel
import Scribe
import Loom.Cookie
import Loom.Session
import Loom.Flash
import Loom.Form

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
