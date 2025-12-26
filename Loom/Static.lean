/-
  Loom.Static - Static file serving
-/
import Citadel
import Staple

namespace Loom

namespace Static

/-- Get MIME type from file extension -/
def mimeType (path : String) : String :=
  let ext := path.splitOn "." |>.getLast? |>.getD "" |>.toLower
  match ext with
  -- Text
  | "html" | "htm" => "text/html; charset=utf-8"
  | "css" => "text/css; charset=utf-8"
  | "js" => "application/javascript; charset=utf-8"
  | "json" => "application/json; charset=utf-8"
  | "xml" => "application/xml; charset=utf-8"
  | "txt" => "text/plain; charset=utf-8"
  | "csv" => "text/csv; charset=utf-8"
  | "md" => "text/markdown; charset=utf-8"
  -- Images
  | "png" => "image/png"
  | "jpg" | "jpeg" => "image/jpeg"
  | "gif" => "image/gif"
  | "svg" => "image/svg+xml"
  | "ico" => "image/x-icon"
  | "webp" => "image/webp"
  -- Fonts
  | "woff" => "font/woff"
  | "woff2" => "font/woff2"
  | "ttf" => "font/ttf"
  | "otf" => "font/otf"
  | "eot" => "application/vnd.ms-fontobject"
  -- Documents
  | "pdf" => "application/pdf"
  | "zip" => "application/zip"
  | "gz" => "application/gzip"
  -- Audio/Video
  | "mp3" => "audio/mpeg"
  | "mp4" => "video/mp4"
  | "webm" => "video/webm"
  | "ogg" => "audio/ogg"
  -- Default
  | _ => "application/octet-stream"

/-- Check if a path is safe (no directory traversal) -/
def isSafePath (path : String) : Bool :=
  !Staple.String.containsSubstr path ".." &&
  !path.startsWith "/" &&
  !Staple.String.containsSubstr path "~"

/-- Configuration for static file serving -/
structure Config where
  /-- Base directory for static files -/
  basePath : String
  /-- Development mode disables caching -/
  devMode : Bool := false
  /-- Cache max-age in seconds (only used when devMode is false) -/
  maxAge : Nat := 3600
  deriving Inhabited

/-- Serve a single file from disk (non-recursive helper) -/
private def serveFileAt (fullPath : String) (devMode : Bool := false) (maxAge : Nat := 3600) : IO (Option Herald.Core.Response) := do
  try
    let contents ← IO.FS.readBinFile fullPath
    let contentType := mimeType fullPath
    -- Get file modification time for ETag
    let metadata ← System.FilePath.metadata fullPath
    let mtime := metadata.modified.sec
    let etag := s!"\"{mtime}\""
    -- Set cache headers based on mode
    let cacheControl := if devMode then
      "no-cache, no-store, must-revalidate"
    else
      s!"public, max-age={maxAge}"
    let resp := Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.ok
      |>.withHeader "Content-Type" contentType
      |>.withHeader "Cache-Control" cacheControl
      |>.withHeader "ETag" etag
      |>.withBody contents
      |>.build
    return some resp
  catch _ =>
    return none

/-- Serve a file from disk -/
def serveFile (basePath : String) (requestPath : String) (devMode : Bool := false) (maxAge : Nat := 3600) : IO (Option Herald.Core.Response) := do
  -- Remove leading slash and sanitize
  let relativePath := if requestPath.startsWith "/" then requestPath.drop 1 else requestPath

  -- Safety check
  if !isSafePath relativePath then
    return none

  let fullPath := s!"{basePath}/{relativePath}"

  -- Check if file exists
  if !(← System.FilePath.pathExists fullPath) then
    return none

  -- Check if it's a directory
  if (← System.FilePath.isDir fullPath) then
    -- Try to serve index.html
    let indexPath := s!"{fullPath}/index.html"
    if !(← System.FilePath.pathExists indexPath) then
      return none
    serveFileAt indexPath devMode maxAge
  else
    serveFileAt fullPath devMode maxAge

/-- Serve a file using config -/
def serveFileWithConfig (config : Config) (requestPath : String) : IO (Option Herald.Core.Response) :=
  serveFile config.basePath requestPath config.devMode config.maxAge

/-- Create a static file handler -/
def handler (basePath : String) (devMode : Bool := false) : Citadel.Handler := fun req => do
  match ← serveFile basePath req.path devMode with
  | some resp => pure resp
  | none => pure Citadel.Response.notFound

/-- Create a static file handler with config -/
def handlerWithConfig (config : Config) : Citadel.Handler := fun req => do
  match ← serveFileWithConfig config req.path with
  | some resp => pure resp
  | none => pure Citadel.Response.notFound

/-- Create static file middleware (tries static first, then passes to handler) -/
def middleware (basePath : String) (devMode : Bool := false) : Citadel.Middleware := fun handler req => do
  -- Only handle GET requests for static files
  if req.method != .GET then
    return ← handler req

  match ← serveFile basePath req.path devMode with
  | some resp => pure resp
  | none => handler req

/-- Create static file middleware with config -/
def middlewareWithConfig (config : Config) : Citadel.Middleware := fun handler req => do
  -- Only handle GET requests for static files
  if req.method != .GET then
    return ← handler req

  match ← serveFileWithConfig config req.path with
  | some resp => pure resp
  | none => handler req

end Static

end Loom
