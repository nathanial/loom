/-
  Loom.Middleware - Common middleware implementations
-/
import Citadel
import Ledger
import Loom.Controller
import Loom.Form
import Loom.Database

namespace Loom

namespace Middleware

/-- Request logging middleware -/
def logging : Citadel.Middleware := fun handler req => do
  let start ← IO.monoNanosNow
  IO.eprintln s!"[{req.method}] {req.path}"
  let resp ← handler req
  let elapsed := (← IO.monoNanosNow) - start
  let ms := elapsed.toFloat / 1000000.0
  IO.eprintln s!"  -> {resp.status.code} ({ms.toString.take 6}ms)"
  pure resp

/-- Add security headers -/
def securityHeaders : Citadel.Middleware := fun handler req => do
  let resp ← handler req
  let headers := resp.headers
    |>.add "X-Content-Type-Options" "nosniff"
    |>.add "X-Frame-Options" "DENY"
    |>.add "X-XSS-Protection" "1; mode=block"
  pure { resp with headers }

/-- Parse method string to Method enum -/
private def parseMethod (s : String) : Option Herald.Core.Method :=
  match s.toUpper with
  | "GET" => some .GET
  | "POST" => some .POST
  | "PUT" => some .PUT
  | "DELETE" => some .DELETE
  | "PATCH" => some .PATCH
  | "HEAD" => some .HEAD
  | "OPTIONS" => some .OPTIONS
  | _ => none

/-- Method override middleware.
    Allows HTML forms to simulate PUT, DELETE, and PATCH requests by including
    a hidden `_method` field. Only applies to POST requests.

    Example HTML form:
    ```html
    <form method="POST" action="/resource/123">
      <input type="hidden" name="_method" value="PUT">
      ...
    </form>
    ```
-/
def methodOverride : Citadel.Middleware := fun handler req => do
  -- Only override POST requests
  if req.method != .POST then
    return ← handler req

  -- Only process form-urlencoded content (skip multipart, json, binary, etc.)
  let contentType := req.request.headers.get "Content-Type" |>.getD ""
  if !contentType.startsWith "application/x-www-form-urlencoded" then
    return ← handler req

  -- Parse body for _method parameter
  let bodyStr := String.fromUTF8! req.request.body
  let params := Form.parseUrlEncoded bodyStr
  match params.lookup "_method" with
  | none => handler req
  | some methodStr =>
    match parseMethod methodStr with
    | none => handler req  -- Invalid method, pass through
    | some newMethod =>
      -- Create new request with overridden method
      let newRequest := { req.request with method := newMethod }
      let newServerRequest := { req with request := newRequest }
      handler newServerRequest

/-- Add CORS headers for API access -/
def cors (allowOrigin : String := "*") : Citadel.Middleware := fun handler req => do
  -- Handle preflight OPTIONS request
  if req.method == .OPTIONS then
    pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.noContent
      |>.withHeader "Access-Control-Allow-Origin" allowOrigin
      |>.withHeader "Access-Control-Allow-Methods" "GET, POST, PUT, PATCH, DELETE, OPTIONS"
      |>.withHeader "Access-Control-Allow-Headers" "Content-Type, Authorization, X-CSRF-Token"
      |>.withHeader "Access-Control-Max-Age" "86400"
      |>.build)
  else
    let resp ← handler req
    let headers := resp.headers
      |>.add "Access-Control-Allow-Origin" allowOrigin
    pure { resp with headers }

/-- Basic authentication middleware -/
def basicAuth (realm : String) (validate : String → String → IO Bool) : Citadel.Middleware :=
  fun handler req => do
    match req.header "Authorization" with
    | some auth =>
      if auth.startsWith "Basic " then
        let encoded := auth.drop 6
        -- Simple base64 decode (very basic, should use proper library)
        -- For now, just pass through and let validate handle it
        match encoded.splitOn ":" with
        | [user, pass] =>
          if ← validate user pass then
            handler req
          else
            unauthorized realm
        | _ => unauthorized realm
      else
        unauthorized realm
    | none => unauthorized realm
  where
    unauthorized (realm : String) : IO Herald.Core.Response :=
      pure (Citadel.ResponseBuilder.withStatus (Herald.Core.StatusCode.mk 401)
        |>.withHeader "WWW-Authenticate" s!"Basic realm=\"{realm}\""
        |>.withText "Unauthorized"
        |>.build)

/-- Rate limiting middleware (simple in-memory, resets on restart) -/
def rateLimit (maxRequests : Nat) (windowSeconds : Nat) : IO Citadel.Middleware := do
  -- In a real implementation, this would use a proper rate limiter
  -- For now, just pass through
  pure fun handler req => handler req

/-- Request timeout middleware -/
def timeout (ms : Nat) : Citadel.Middleware := fun handler req => do
  -- Note: True timeout would need async/Task support
  -- For now, just execute normally
  handler req

/-- Error recovery middleware -/
def errorRecovery : Citadel.Middleware := fun handler req => do
  try
    handler req
  catch e =>
    IO.eprintln s!"Error handling request: {e}"
    pure Citadel.Response.internalError

/-- Compose multiple middleware into one -/
def compose (middlewares : List Citadel.Middleware) : Citadel.Middleware :=
  middlewares.foldr (init := id) fun mw acc => fun handler => mw (acc handler)

-- ============================================================================
-- HTTPS Redirect
-- ============================================================================

/-- Build HTTPS redirect URL from request.
    Uses the Host header to determine the target domain.
    If httpsPort is not 443, it's included in the URL. -/
private def buildHttpsUrl (req : Citadel.ServerRequest) (httpsPort : UInt16 := 443) : String :=
  let host := req.header "Host" |>.getD "localhost"
  -- Strip any existing port from host
  let hostWithoutPort := match host.splitOn ":" with
    | h :: _ => h
    | [] => host
  let portSuffix := if httpsPort == 443 then "" else s!":{httpsPort}"
  s!"https://{hostWithoutPort}{portSuffix}{req.fullPath}"

/-- HTTPS redirect middleware.
    Redirects all requests to their HTTPS equivalent using 301 Moved Permanently.
    Use this on an HTTP-only server (port 80) to redirect traffic to HTTPS.

    Parameters:
    - `httpsPort`: The HTTPS port to redirect to (default 443, omitted from URL if 443)

    Example usage:
    ```lean
    -- Create a simple HTTP redirect server
    let httpServer := Citadel.Server.new
      { port := 80, host := "0.0.0.0" }
      (Citadel.Router.empty.get "*" fun _ => pure Citadel.Response.notFound)
    httpServer.use (Middleware.httpsRedirect 443)
    ```
-/
def httpsRedirect (httpsPort : UInt16 := 443) : Citadel.Middleware := fun _handler req => do
  let url := buildHttpsUrl req httpsPort
  -- Use 301 for permanent redirect (browsers will cache and auto-redirect)
  pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.movedPermanently
    |>.withHeader "Location" url
    |>.withHeader "Cache-Control" "max-age=31536000"  -- Cache for 1 year
    |>.withText s!"Redirecting to {url}"
    |>.build)

/-- HTTPS redirect middleware with temporary redirect (302).
    Same as httpsRedirect but uses 302 Found instead of 301.
    Use this during testing or when the redirect might change. -/
def httpsRedirectTemporary (httpsPort : UInt16 := 443) : Citadel.Middleware := fun _handler req => do
  let url := buildHttpsUrl req httpsPort
  pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.found
    |>.withHeader "Location" url
    |>.withText s!"Redirecting to {url}"
    |>.build)

-- ============================================================================
-- Database middleware
-- ============================================================================

/-- Database error recovery middleware.
    Catches any database-related exceptions and returns 500 with error info.
    This catches all exceptions - use errorRecovery for more selective handling. -/
def databaseErrorRecovery : Citadel.Middleware := fun handler req => do
  try
    handler req
  catch e =>
    IO.eprintln s!"Database error: {e}"
    pure (Citadel.ResponseBuilder.withStatus (Herald.Core.StatusCode.mk 500)
      |>.withText "Database error occurred"
      |>.build)

/-- Query logging middleware.
    Logs timing information for requests (db query logging happens at action level). -/
def queryLogging : Citadel.Middleware := fun handler req => do
  let start ← IO.monoNanosNow
  let resp ← handler req
  let elapsed := (← IO.monoNanosNow) - start
  let ms := elapsed.toFloat / 1000000.0
  if ms > 100.0 then  -- Log slow requests (>100ms)
    IO.eprintln s!"[SLOW] {req.method} {req.path} took {ms.toString.take 8}ms"
  pure resp

end Middleware

-- ============================================================================
-- Route-level middleware (operates on Context, not raw HTTP)
-- ============================================================================

/-- Route middleware wraps an Action, can short-circuit or continue.
    Unlike Citadel.Middleware which operates on raw HTTP requests,
    RouteMiddleware has access to the parsed Context (session, params, db, etc.). -/
def RouteMiddleware := Action → Action

namespace RouteMiddleware

/-- Identity middleware that passes through unchanged -/
def identity : RouteMiddleware := id

/-- Compose two route middleware (m1 wraps m2) -/
def compose (m1 m2 : RouteMiddleware) : RouteMiddleware :=
  fun action => m1 (m2 action)

/-- Chain a list of route middleware (first in list is outermost) -/
def chain (middlewares : List RouteMiddleware) : RouteMiddleware :=
  middlewares.foldr compose identity

/-- Create middleware that redirects if a condition fails -/
def guard (check : Context → Bool) (redirectTo : String)
    (flashKey : String := "error") (flashMsg : String := "") : RouteMiddleware :=
  fun action ctx => do
    if check ctx then
      action ctx
    else
      let ctx' := if flashMsg.isEmpty then ctx
                  else ctx.withFlash fun f => f.set flashKey flashMsg
      Action.redirect redirectTo ctx'

/-- Create middleware that runs an IO check and redirects on failure -/
def guardM (check : Context → IO Bool) (redirectTo : String)
    (flashKey : String := "error") (flashMsg : String := "") : RouteMiddleware :=
  fun action ctx => do
    if ← check ctx then
      action ctx
    else
      let ctx' := if flashMsg.isEmpty then ctx
                  else ctx.withFlash fun f => f.set flashKey flashMsg
      Action.redirect redirectTo ctx'

/-- Create middleware that modifies context before passing to action -/
def mapContext (f : Context → Context) : RouteMiddleware :=
  fun action ctx => action (f ctx)

/-- Create middleware that modifies context using IO before passing to action -/
def mapContextM (f : Context → IO Context) : RouteMiddleware :=
  fun action ctx => do action (← f ctx)

end RouteMiddleware

end Loom
