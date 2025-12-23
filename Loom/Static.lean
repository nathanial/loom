/-
  Loom.Static - Static file serving
-/
import Citadel

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

/-- Check if a string contains a substring -/
private def containsSubstr (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

/-- Check if a path is safe (no directory traversal) -/
def isSafePath (path : String) : Bool :=
  !containsSubstr path ".." &&
  !path.startsWith "/" &&
  !containsSubstr path "~"

/-- Serve a single file from disk (non-recursive helper) -/
private def serveFileAt (fullPath : String) : IO (Option Herald.Core.Response) := do
  try
    let contents ← IO.FS.readBinFile fullPath
    let contentType := mimeType fullPath
    let resp := Citadel.ResponseBuilder.withStatus Herald.Core.StatusCode.ok
      |>.withHeader "Content-Type" contentType
      |>.withHeader "Cache-Control" "public, max-age=3600"
      |>.withBody contents
      |>.build
    return some resp
  catch _ =>
    return none

/-- Serve a file from disk -/
def serveFile (basePath : String) (requestPath : String) : IO (Option Herald.Core.Response) := do
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
    serveFileAt indexPath
  else
    serveFileAt fullPath

/-- Create a static file handler -/
def handler (basePath : String) : Citadel.Handler := fun req => do
  match ← serveFile basePath req.path with
  | some resp => pure resp
  | none => pure Citadel.Response.notFound

/-- Create static file middleware (tries static first, then passes to handler) -/
def middleware (basePath : String) : Citadel.Middleware := fun handler req => do
  -- Only handle GET requests for static files
  if req.method != .GET then
    return ← handler req

  match ← serveFile basePath req.path with
  | some resp => pure resp
  | none => handler req

end Static

end Loom
