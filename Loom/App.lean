/-
  Loom.App - Application container that ties everything together
-/
import Citadel
import Ledger
import Chronicle
import Loom.Controller
import Loom.ActionM
import Loom.Router
import Loom.Middleware
import Loom.Static
import Loom.Database
import Loom.SSE
import Loom.Stencil.Config
import Loom.Stencil.Manager

namespace Loom

/-- Application builder for fluent route registration -/
structure App where
  config : AppConfig
  routes : Routes := Routes.empty
  middlewares : List Citadel.Middleware := []
  /-- Whether SSE is enabled for this application -/
  sseEnabled : Bool := false
  /-- SSE routes: (pattern, topic) -/
  sseRoutes : List (String × String) := []
  /-- Application logger for action-level logging -/
  logger : Option Chronicle.MultiLogger := none
  /-- Stencil template engine configuration -/
  stencilConfig : Option Stencil.Config := none

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

/-- Add a named route with optional middleware (accepts Action or ActionM Response) -/
def route [ToAction α] (app : App) (name : String) (method : Herald.Core.Method) (pattern : String)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  { app with routes := app.routes.add name method pattern middleware action }

/-- Add a GET route with optional middleware -/
def get [ToAction α] (app : App) (pattern : String) (name : String)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  app.route name .GET pattern middleware action

/-- Add a POST route with optional middleware -/
def post [ToAction α] (app : App) (pattern : String) (name : String)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  app.route name .POST pattern middleware action

/-- Add a PUT route with optional middleware -/
def put [ToAction α] (app : App) (pattern : String) (name : String)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  app.route name .PUT pattern middleware action

/-- Add a DELETE route with optional middleware -/
def delete [ToAction α] (app : App) (pattern : String) (name : String)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  app.route name .DELETE pattern middleware action

/-- Add a PATCH route with optional middleware -/
def patch [ToAction α] (app : App) (pattern : String) (name : String)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  app.route name .PATCH pattern middleware action

/-! ## Type-safe route registration -/

/-- Typeclass for route types that provide pattern and name extraction -/
class HasRouteInfo (R : Type) where
  /-- Get the URL pattern with :param placeholders (e.g., "/kanban/column/:id") -/
  pattern : R → String
  /-- Get the route name in snake_case (e.g., "kanban_get_column") -/
  routeName : R → String

/-- Add a route using type-safe route info with optional middleware.
    Extracts pattern and name from the route type automatically. -/
def route' [ToAction α] [HasRouteInfo R] (app : App) (method : Herald.Core.Method) (r : R)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  app.route (HasRouteInfo.routeName r) method (HasRouteInfo.pattern r) middleware action

/-- Add a GET route using type-safe route info with optional middleware -/
def get' [ToAction α] [HasRouteInfo R] (app : App) (r : R)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  app.route' .GET r middleware action

/-- Add a POST route using type-safe route info with optional middleware -/
def post' [ToAction α] [HasRouteInfo R] (app : App) (r : R)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  app.route' .POST r middleware action

/-- Add a PUT route using type-safe route info with optional middleware -/
def put' [ToAction α] [HasRouteInfo R] (app : App) (r : R)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  app.route' .PUT r middleware action

/-- Add a DELETE route using type-safe route info with optional middleware -/
def delete' [ToAction α] [HasRouteInfo R] (app : App) (r : R)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  app.route' .DELETE r middleware action

/-- Add a PATCH route using type-safe route info with optional middleware -/
def patch' [ToAction α] [HasRouteInfo R] (app : App) (r : R)
    (middleware : List RouteMiddleware := []) (action : α) : App :=
  app.route' .PATCH r middleware action

/-- Configure database with a custom connection factory.
    This automatically adds database error recovery middleware. -/
def withDatabase (app : App) (factory : Database.ConnectionFactory) : App :=
  let dbConfig : Database.DbConfig := { factory := factory }
  { app with
    config := { app.config with dbConfig := some dbConfig }
    middlewares := app.middlewares ++ [Middleware.databaseErrorRecovery] }

/-- Configure database with the default in-memory connection factory. -/
def withDefaultDatabase (app : App) : App :=
  app.withDatabase Database.defaultFactory

/-- Configure database with persistent JSONL storage.
    Transactions are automatically written to the journal file and
    replayed on startup to restore state. -/
def withPersistentDatabase (app : App) (journalPath : System.FilePath) : App :=
  let dbConfig := Database.DbConfig.withPersistence journalPath
  { app with
    config := { app.config with dbConfig := some dbConfig }
    middlewares := app.middlewares ++ [Middleware.databaseErrorRecovery] }

/-! ## Server-Sent Events (SSE) Configuration -/

/-- Set the application logger for action-level logging -/
def withLogger (app : App) (logger : Chronicle.MultiLogger) : App :=
  { app with logger := some logger }

/-- Enable Stencil template engine with configuration -/
def withStencil (app : App) (config : Stencil.Config) : App :=
  { app with stencilConfig := some config }

/-- Enable Stencil template engine with default configuration -/
def withStencilDefault (app : App) : App :=
  app.withStencil Stencil.Config.default

/-- Enable SSE support for the application.
    This allows the app to broadcast real-time events to connected clients. -/
def withSSE (app : App) : App :=
  { app with sseEnabled := true }

/-- Register an SSE endpoint at the given path pattern.
    Clients can connect to this endpoint to receive server-sent events.
    The topic parameter determines which events the client will receive. -/
def sseEndpoint (app : App) (pattern : String) (topic : String := "default") : App :=
  { app with
    sseEnabled := true
    sseRoutes := app.sseRoutes ++ [(pattern, topic)] }

/-- Parse cookies from request -/
private def parseCookies (req : Citadel.ServerRequest) : List (String × String) :=
  match req.header "Cookie" with
  | some cookieHeader => Cookie.parse cookieHeader
  | none => []

/-- Load session from request cookies -/
private def loadSession (req : Citadel.ServerRequest) (config : AppConfig) : Session :=
  let cookies := parseCookies req
  match cookies.lookup config.sessionCookieName with
  | some encoded => Session.decode encoded config.secretKey |>.getD Session.empty
  | none => Session.empty

/-- Build context from request -/
private def buildContext (req : Citadel.ServerRequest) (config : AppConfig)
    (cachedPersistentDbRef : Option (IO.Ref Ledger.Persist.PersistentConnection) := none)
    (logger : Option Chronicle.MultiLogger := none)
    (stencilManagerRef : Option (IO.Ref Stencil.Manager) := none) : IO Context := do
  -- Load session
  let session := loadSession req config

  -- Load flash from session
  let (flash, session) := Flash.fromSession session

  -- Parse parameters (path params would come from router, for now just query + body)
  let queryParams := Form.parseQueryString req.path

  -- Parse body based on content type
  let contentType := req.header "Content-Type"
  let (bodyParams, multipartData) :=
    if req.method == .GET || req.method == .HEAD then
      ([], none)
    else if Multipart.isMultipart (contentType.getD "") then
      match Multipart.parse (contentType.getD "") req.body with
      | some mpd => (mpd.toParams, some mpd)
      | none => ([], none)
    else
      (Form.parseUrlEncoded req.bodyString, none)

  let params := queryParams ++ bodyParams

  -- Generate CSRF token
  let csrfToken := Form.generateCsrfToken config.secretKey session

  -- Use cached persistent connection ref if available, otherwise create new connection
  let (db, persistentDb) ← match cachedPersistentDbRef with
    | some ref =>
      let pc ← ref.get  -- Read current state from shared ref
      pure (none, some pc)
    | none =>
      -- Fall back to per-request connection (only for non-persistent or tests)
      match config.dbConfig with
      | some dbConfig =>
        match dbConfig.journalPath with
        | some path =>
          let pc ← Ledger.Persist.PersistentConnection.create path
          pure (none, some pc)
        | none =>
          let conn ← dbConfig.factory
          pure (some conn, none)
      | none => pure (none, none)

  pure { request := req
       , session := session
       , flash := flash
       , params := params
       , config := config
       , csrfToken := csrfToken
       , db := db
       , persistentDb := persistentDb
       , logger := logger
       , multipartData := multipartData
       , stencilManager := stencilManagerRef
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

/-- Count parameter segments in a pattern -/
private def countParams (pattern : String) : Nat :=
  pattern.splitOn "/" |>.filter (·.startsWith ":") |>.length

/-- Count literal (non-parameter) segments in a pattern -/
private def countLiterals (pattern : String) : Nat :=
  pattern.splitOn "/" |>.filter (fun s => s != "" && !s.startsWith ":") |>.length

/-- Find matching route for request (prefers more specific routes) -/
private def findRoute (routes : Routes) (method : Herald.Core.Method) (path : String) : Option (NamedRoute × List (String × String)) :=
  -- Remove query string from path
  let cleanPath := Form.pathWithoutQuery path
  -- Collect all matching routes with their extracted params
  let matchingRoutes := routes.routes.filterMap fun route =>
    if route.method != method then none
    else matchRoute route.pattern cleanPath |>.map (route, ·)
  -- Sort by specificity: more literals is better, then fewer params is better
  let sorted := matchingRoutes.toArray.qsort fun (r1, _) (r2, _) =>
    let lit1 := countLiterals r1.pattern
    let lit2 := countLiterals r2.pattern
    if lit1 != lit2 then lit1 > lit2  -- More literals = more specific
    else countParams r1.pattern < countParams r2.pattern  -- Fewer params = more specific
  if h : sorted.size > 0 then some sorted[0] else none

/-- Create the main handler -/
def toHandler (app : App)
    (cachedPersistentDbRef : Option (IO.Ref Ledger.Persist.PersistentConnection) := none)
    (stencilManagerRef : Option (IO.Ref Stencil.Manager) := none) : Citadel.Handler := fun req => do
  -- Build initial context (reads current connection from ref if available)
  let ctx ← buildContext req app.config cachedPersistentDbRef app.logger stencilManagerRef

  -- Find matching route
  match findRoute app.routes req.method req.path with
  | none =>
    -- Try static file if configured
    match app.config.staticPath with
    | some staticPath =>
      match ← Static.serveFile staticPath req.path app.config.devMode with
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
      -- Apply route-level middleware chain, then execute action
      let wrappedAction := RouteMiddleware.chain route.middleware route.action
      let (resp, ctx') ← wrappedAction ctx
      -- Write updated connection back to shared ref (if using cached db)
      match cachedPersistentDbRef, ctx'.persistentDb with
      | some ref, some updatedPc => ref.set updatedPc
      | _, _ => pure ()
      -- Finalize response with session from modified context
      pure (finalizeResponse ctx' resp)

/-- Create Citadel server from app (without SSE - use for non-SSE apps) -/
def toServerBase (app : App)
    (cachedPersistentDbRef : Option (IO.Ref Ledger.Persist.PersistentConnection) := none)
    (stencilManagerRef : Option (IO.Ref Stencil.Manager) := none)
    (host : String := "0.0.0.0") (port : UInt16 := 3000) : Citadel.Server :=
  let handler := app.toHandler cachedPersistentDbRef stencilManagerRef

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

/-- Create Citadel server from app with SSE support -/
def toServerWithSSE (app : App) (manager : Citadel.SSE.ConnectionManager)
    (cachedPersistentDbRef : Option (IO.Ref Ledger.Persist.PersistentConnection) := none)
    (stencilManagerRef : Option (IO.Ref Stencil.Manager) := none)
    (host : String := "0.0.0.0") (port : UInt16 := 3000) : Citadel.Server :=
  let handler := app.toHandler cachedPersistentDbRef stencilManagerRef

  -- Compose middleware
  let composedMiddleware := Middleware.compose app.middlewares
  let finalHandler := composedMiddleware handler

  -- Create server with config
  let config : Citadel.ServerConfig := {
    host := host
    port := port
  }

  -- Add routes and SSE configuration
  let server := Citadel.Server.create config
    |>.withSSE manager
    |>.route .GET "/*" finalHandler
    |>.route .POST "/*" finalHandler
    |>.route .PUT "/*" finalHandler
    |>.route .DELETE "/*" finalHandler
    |>.route .PATCH "/*" finalHandler
    |>.route .OPTIONS "/*" finalHandler

  -- Register SSE routes
  app.sseRoutes.foldl (fun s (pattern, topic) => s.sseRoute pattern topic) server

/-- File entry for static file watching -/
private structure StaticFileEntry where
  path : System.FilePath
  mtime : UInt64
  deriving Inhabited

/-- Get file modification time -/
private def getFileMtime (path : System.FilePath) : IO UInt64 := do
  let metadata ← path.metadata
  pure metadata.modified.sec.toNat.toUInt64

/-- Recursively list all files in a directory -/
private partial def walkDir (dir : System.FilePath) : IO (List System.FilePath) := do
  let mut files : List System.FilePath := []
  if ← dir.isDir then
    for entry in ← dir.readDir do
      if ← entry.path.isDir then
        let subFiles ← walkDir entry.path
        files := files ++ subFiles
      else
        files := entry.path :: files
  pure files

/-- Start a background watcher for static files (CSS, JS, etc.).
    Polls for mtime changes and calls onChanged with the relative file path. -/
def startStaticWatcher (dir : String) (extension : String) (onChanged : String → IO Unit)
    : IO (Task (Except IO.Error Unit)) := do
  let dirPath := System.FilePath.mk dir
  -- Build initial file map
  let allFiles ← walkDir dirPath
  let cssFiles := allFiles.filter fun p => p.toString.endsWith extension
  let mut fileMap : Std.HashMap String StaticFileEntry := {}
  for path in cssFiles do
    let mtime ← getFileMtime path
    let relPath := path.toString.drop (dir.length + 1)  -- Remove dir prefix
    fileMap := fileMap.insert relPath { path, mtime }

  let fileMapRef ← IO.mkRef fileMap

  IO.asTask do
    while true do
      IO.sleep (500 : UInt32)  -- Poll every 500ms
      let currentMap ← fileMapRef.get
      let mut newMap := currentMap
      for (relPath, entry) in currentMap.toList do
        if ← entry.path.pathExists then
          let currentMtime ← getFileMtime entry.path
          if currentMtime != entry.mtime then
            IO.println s!"Hot reload: {extension} {relPath}"
            newMap := newMap.insert relPath { entry with mtime := currentMtime }
            onChanged relPath
      fileMapRef.set newMap

/-- Run the application -/
def run (app : App) (host : String := "0.0.0.0") (port : UInt16 := 3000) : IO Unit := do
  -- Initialize persistent database connection once at startup (not per-request!)
  -- Use IO.Ref to share mutable state across requests
  let cachedDbRef ← match app.config.dbConfig with
    | some dbConfig =>
      match dbConfig.journalPath with
      | some path =>
        let pc ← Ledger.Persist.PersistentConnection.create path
        IO.println s!"Database: {pc.db.size} datoms loaded from {path}"
        let ref ← IO.mkRef pc
        pure (some ref)
      | none => pure none
    | none => pure none

  -- Initialize Stencil template manager if configured
  let stencilRef ← match app.stencilConfig with
    | some config =>
      IO.println s!"Templates: Discovering from {config.templateDir}/"
      let manager ← Stencil.Manager.discover config
      IO.println s!"Templates: {manager.templateCount} templates, {manager.partialCount} partials, {manager.layoutCount} layouts"
      let ref ← IO.mkRef manager
      pure (some ref)
    | none => pure none

  if app.sseEnabled then
    -- Create SSE connection manager
    let manager ← Citadel.SSE.ConnectionManager.create

    -- Initialize the global SSE publisher
    SSE.setup manager

    -- Start hot reload watcher if stencil is enabled with hot reload
    match stencilRef with
    | some ref =>
      let mgr ← ref.get
      if mgr.config.hotReload then
        let _ ← Stencil.Manager.startWatcher ref do
          SSE.publishEvent "hot-reload" "reload" ""
        IO.println "Hot reload: Watching for template changes"
    | none => pure ()

    -- Start CSS hot reload watcher if static path is configured
    match app.config.staticPath with
    | some staticPath =>
      let cssDir := staticPath ++ "/css"
      if ← (System.FilePath.mk cssDir).pathExists then
        let _ ← startStaticWatcher cssDir ".css" fun relPath => do
          SSE.publishEvent "hot-reload" "css" s!"/css/{relPath}"
        IO.println s!"Hot reload: Watching {cssDir} for CSS changes"
    | none => pure ()

    -- Create server with SSE support (using cached db ref)
    let server := app.toServerWithSSE manager cachedDbRef stencilRef host port

    IO.println s!"Loom starting on http://{host}:{port}"
    if app.sseRoutes.length > 0 then
      IO.println s!"SSE endpoints: {app.sseRoutes.map Prod.fst}"
    server.run
  else
    let server := app.toServerBase cachedDbRef stencilRef host port
    IO.println s!"Loom starting on http://{host}:{port}"
    server.run

/-- Run the application with HTTPS and automatic HTTP→HTTPS redirect.
    Starts two servers:
    - HTTPS server on the specified port (default 443)
    - HTTP redirect server on httpPort (default 80)

    Example:
    ```lean
    app.runWithHttpsRedirect
      (host := "0.0.0.0")
      (httpsPort := 443)
      (httpPort := 80)
      (tlsConfig := { certFile := "/path/to/cert.pem", keyFile := "/path/to/key.pem" })
    ```
-/
def runWithHttpsRedirect (app : App)
    (host : String := "0.0.0.0")
    (httpsPort : UInt16 := 443)
    (httpPort : UInt16 := 80)
    (tlsConfig : Citadel.TlsConfig) : IO Unit := do
  -- Initialize persistent database connection once at startup
  let cachedDbRef ← match app.config.dbConfig with
    | some dbConfig =>
      match dbConfig.journalPath with
      | some path =>
        let pc ← Ledger.Persist.PersistentConnection.create path
        IO.println s!"Database: {pc.db.size} datoms loaded from {path}"
        let ref ← IO.mkRef pc
        pure (some ref)
      | none => pure none
    | none => pure none

  -- Initialize Stencil template manager if configured
  let stencilRef ← match app.stencilConfig with
    | some config =>
      IO.println s!"Templates: Discovering from {config.templateDir}/"
      let manager ← Stencil.Manager.discover config
      IO.println s!"Templates: {manager.templateCount} templates, {manager.partialCount} partials, {manager.layoutCount} layouts"
      let ref ← IO.mkRef manager
      pure (some ref)
    | none => pure none

  -- Create HTTP redirect server (runs in background)
  let redirectHandler : Citadel.Handler := fun req => do
    let hostHeader := req.header "Host" |>.getD "localhost"
    let hostWithoutPort := match hostHeader.splitOn ":" with
      | h :: _ => h
      | [] => hostHeader
    let portSuffix := if httpsPort == 443 then "" else s!":{httpsPort}"
    let url := s!"https://{hostWithoutPort}{portSuffix}{req.fullPath}"
    pure (Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.movedPermanently
      |>.withHeader "Location" url
      |>.withHeader "Cache-Control" "max-age=31536000"
      |>.withText s!"Redirecting to {url}"
      |>.build)

  let httpConfig : Citadel.ServerConfig := { host := host, port := httpPort }
  let httpServer := Citadel.Server.create httpConfig
    |>.route .GET "/*" redirectHandler
    |>.route .POST "/*" redirectHandler
    |>.route .PUT "/*" redirectHandler
    |>.route .DELETE "/*" redirectHandler
    |>.route .PATCH "/*" redirectHandler
    |>.route .OPTIONS "/*" redirectHandler

  -- Start HTTP redirect server in background task
  let _ ← IO.asTask (prio := .dedicated) do
    IO.println s!"HTTP redirect server on http://{host}:{httpPort} → https://...:{httpsPort}"
    httpServer.run

  -- Build and run HTTPS server (main thread)
  let httpsConfig : Citadel.ServerConfig := {
    host := host
    port := httpsPort
    tls := some tlsConfig
  }

  if app.sseEnabled then
    let manager ← Citadel.SSE.ConnectionManager.create
    SSE.setup manager
    let handler := app.toHandler cachedDbRef stencilRef
    let composedMiddleware := Middleware.compose app.middlewares
    let finalHandler := composedMiddleware handler

    let server := Citadel.Server.create httpsConfig
      |>.withSSE manager
      |>.route .GET "/*" finalHandler
      |>.route .POST "/*" finalHandler
      |>.route .PUT "/*" finalHandler
      |>.route .DELETE "/*" finalHandler
      |>.route .PATCH "/*" finalHandler
      |>.route .OPTIONS "/*" finalHandler

    let server := app.sseRoutes.foldl (fun s (pattern, topic) => s.sseRoute pattern topic) server

    IO.println s!"HTTPS server on https://{host}:{httpsPort}"
    if app.sseRoutes.length > 0 then
      IO.println s!"SSE endpoints: {app.sseRoutes.map Prod.fst}"
    server.run
  else
    let handler := app.toHandler cachedDbRef stencilRef
    let composedMiddleware := Middleware.compose app.middlewares
    let finalHandler := composedMiddleware handler

    let server := Citadel.Server.create httpsConfig
      |>.route .GET "/*" finalHandler
      |>.route .POST "/*" finalHandler
      |>.route .PUT "/*" finalHandler
      |>.route .DELETE "/*" finalHandler
      |>.route .PATCH "/*" finalHandler
      |>.route .OPTIONS "/*" finalHandler

    IO.println s!"HTTPS server on https://{host}:{httpsPort}"
    server.run

/-- URL helpers for the app -/
def urlHelpers (app : App) (baseUrl : String := "") : UrlHelpers :=
  UrlHelpers.create app.routes baseUrl

end App

/-- DSL for building applications -/
def app (config : AppConfig) : App :=
  App.create config

end Loom
