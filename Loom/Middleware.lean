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

end Loom
