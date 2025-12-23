/-
  Loom.App - Application container that ties everything together
-/
import Citadel
import Loom.Controller
import Loom.Router
import Loom.Middleware
import Loom.Static

namespace Loom

/-- Application builder for fluent route registration -/
structure App where
  config : AppConfig
  routes : Routes := Routes.empty
  middlewares : List Citadel.Middleware := []

namespace App

/-- Create a new application with config -/
def create (config : AppConfig) : App :=
  { config, routes := Routes.empty, middlewares := [] }

/-- Create with just a secret key -/
def withSecret (secret : String) : App :=
  create { secretKey := secret.toUTF8 }

/-- Add middleware to the application -/
def use (app : App) (mw : Citadel.Middleware) : App :=
  { app with middlewares := app.middlewares ++ [mw] }

/-- Add a named route -/
def route (app : App) (name : String) (method : Herald.Core.Method) (pattern : String) (action : Action) : App :=
  { app with routes := app.routes.add name method pattern action }

/-- Add a GET route -/
def get (app : App) (pattern : String) (name : String) (action : Action) : App :=
  app.route name .GET pattern action

/-- Add a POST route -/
def post (app : App) (pattern : String) (name : String) (action : Action) : App :=
  app.route name .POST pattern action

/-- Add a PUT route -/
def put (app : App) (pattern : String) (name : String) (action : Action) : App :=
  app.route name .PUT pattern action

/-- Add a DELETE route -/
def delete (app : App) (pattern : String) (name : String) (action : Action) : App :=
  app.route name .DELETE pattern action

/-- Add a PATCH route -/
def patch (app : App) (pattern : String) (name : String) (action : Action) : App :=
  app.route name .PATCH pattern action

/-- Parse cookies from request -/
private def parseCookies (req : Citadel.ServerRequest) : List (String × String) :=
  match req.header "Cookie" with
  | some cookieHeader => Cookie.parse cookieHeader
  | none => []

/-- Load session from request cookies -/
private def loadSession (req : Citadel.ServerRequest) (config : AppConfig) : Session :=
  let cookies := parseCookies req
  match cookies.lookup config.sessionCookieName with
  | some encoded =>
    Session.decode encoded config.secretKey |>.getD Session.empty
  | none => Session.empty

/-- Build context from request -/
private def buildContext (req : Citadel.ServerRequest) (config : AppConfig) : Context :=
  -- Load session
  let session := loadSession req config

  -- Load flash from session
  let (flash, session) := Flash.fromSession session

  -- Parse parameters (path params would come from router, for now just query + body)
  let queryParams := Form.parseQueryString req.path
  let bodyParams :=
    if req.method != .GET && req.method != .HEAD then
      Form.parseUrlEncoded req.bodyString
    else
      []
  let params := queryParams ++ bodyParams

  -- Generate CSRF token
  let csrfToken := Form.generateCsrfToken config.secretKey session

  { request := req
  , session := session
  , flash := flash
  , params := params
  , config := config
  , csrfToken := csrfToken
  }

/-- Add path parameters to context -/
private def addPathParams (ctx : Context) (pathParams : List (String × String)) : Context :=
  { ctx with params := pathParams ++ ctx.params }

/-- Finalize response with session cookie -/
private def finalizeResponse (ctx : Context) (resp : Herald.Core.Response) : Herald.Core.Response :=
  -- Save flash to session
  let session := Flash.toSession ctx.flash ctx.session

  -- Encode session to cookie
  let encoded := Session.encode session ctx.config.secretKey

  -- Build cookie
  let cookie : Cookie := {
    name := ctx.config.sessionCookieName
    value := encoded
    path := some "/"
    httpOnly := true
    sameSite := some "Lax"
  }

  -- Add Set-Cookie header
  let headers := resp.headers.add "Set-Cookie" (Cookie.build cookie)
  { resp with headers }

/-- Check if request needs CSRF validation -/
private def needsCsrfValidation (method : Herald.Core.Method) : Bool :=
  method == .POST || method == .PUT || method == .PATCH || method == .DELETE

/-- Validate CSRF token from request -/
private def validateCsrf (ctx : Context) : Bool :=
  if !ctx.config.csrfEnabled then true
  else if !needsCsrfValidation ctx.request.method then true
  else
    -- Check form field
    match ctx.params.lookup ctx.config.csrfFieldName with
    | some token => Form.validateCsrfToken token ctx.config.secretKey ctx.session
    | none =>
      -- Check header (for AJAX)
      match ctx.request.header "X-CSRF-Token" with
      | some token => Form.validateCsrfToken token ctx.config.secretKey ctx.session
      | none => false

/-- Match a route pattern against a path, extracting parameters -/
private def matchRoute (pattern : String) (path : String) : Option (List (String × String)) :=
  let patternParts := pattern.splitOn "/" |>.filter (· != "")
  let pathParts := path.splitOn "/" |>.filter (· != "")

  if patternParts.length != pathParts.length then none
  else
    let pairs := patternParts.zip pathParts
    let result := pairs.foldl (init := some []) fun acc (pat, actual) =>
      match acc with
      | none => none
      | some params =>
        if pat.startsWith ":" then
          -- Parameter segment
          let paramName := pat.drop 1
          some ((paramName, actual) :: params)
        else if pat == actual then
          -- Literal match
          some params
        else
          none
    result.map List.reverse

/-- Find matching route for request -/
private def findRoute (routes : Routes) (method : Herald.Core.Method) (path : String) : Option (NamedRoute × List (String × String)) :=
  -- Remove query string from path
  let cleanPath := Form.pathWithoutQuery path
  routes.routes.findSome? fun route =>
    if route.method != method then none
    else matchRoute route.pattern cleanPath |>.map (route, ·)

/-- Create the main handler -/
def toHandler (app : App) : Citadel.Handler := fun req => do
  -- Build initial context
  let ctx := buildContext req app.config

  -- Find matching route
  match findRoute app.routes req.method req.path with
  | none =>
    -- Try static file if configured
    match app.config.staticPath with
    | some staticPath =>
      match ← Static.serveFile staticPath req.path with
      | some resp => pure resp
      | none => pure Citadel.Response.notFound
    | none => pure Citadel.Response.notFound
  | some (route, pathParams) =>
    -- Add path params to context
    let ctx := addPathParams ctx pathParams

    -- CSRF validation
    if !validateCsrf ctx then
      pure (Citadel.ResponseBuilder.withStatus (Herald.Core.StatusCode.mk 403)
        |>.withText "CSRF token validation failed"
        |>.build)
    else
      -- Execute action
      let resp ← route.action ctx
      -- Finalize response with session
      pure (finalizeResponse ctx resp)

/-- Create Citadel server from app -/
def toServer (app : App) (host : String := "0.0.0.0") (port : UInt16 := 3000) : Citadel.Server :=
  let handler := app.toHandler

  -- Compose middleware
  let composedMiddleware := Middleware.compose app.middlewares
  let finalHandler := composedMiddleware handler

  -- Create server with config and add a catch-all route
  let config : Citadel.ServerConfig := {
    host := host
    port := port
  }

  Citadel.Server.create config
    |>.route .GET "/*" finalHandler
    |>.route .POST "/*" finalHandler
    |>.route .PUT "/*" finalHandler
    |>.route .DELETE "/*" finalHandler
    |>.route .PATCH "/*" finalHandler
    |>.route .OPTIONS "/*" finalHandler

/-- Run the application -/
def run (app : App) (host : String := "0.0.0.0") (port : UInt16 := 3000) : IO Unit := do
  let server := app.toServer host port
  IO.println s!"Loom starting on http://{host}:{port}"
  server.run

/-- URL helpers for the app -/
def urlHelpers (app : App) (baseUrl : String := "") : UrlHelpers :=
  UrlHelpers.create app.routes baseUrl

end App

/-- DSL for building applications -/
def app (config : AppConfig) : App :=
  App.create config

end Loom
