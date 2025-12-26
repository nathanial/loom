/-
  Loom.Multipart - Multipart form data parsing (RFC 7578)
-/
namespace Loom

/-- A parsed multipart part (file or form field) -/
structure MultipartPart where
  /-- Field name from Content-Disposition -/
  name : String
  /-- Original filename (for file uploads) -/
  filename : Option String := none
  /-- Content type (for file uploads) -/
  contentType : Option String := none
  /-- Part content as bytes -/
  content : ByteArray
  deriving Inhabited

/-- Parsed multipart form data -/
structure MultipartData where
  /-- All parts parsed from the request -/
  parts : List MultipartPart
  deriving Inhabited

namespace MultipartData

/-- Get a form field value by name (first match, non-file parts only) -/
def getField (m : MultipartData) (name : String) : Option String :=
  m.parts.findSome? fun p =>
    if p.name == name && p.filename.isNone then
      some (String.fromUTF8! p.content)
    else
      none

/-- Get all fields with a given name -/
def getFields (m : MultipartData) (name : String) : List String :=
  m.parts.filterMap fun p =>
    if p.name == name && p.filename.isNone then
      some (String.fromUTF8! p.content)
    else
      none

/-- Get a file upload by field name -/
def getFile (m : MultipartData) (name : String) : Option MultipartPart :=
  m.parts.find? fun p => p.name == name && p.filename.isSome

/-- Get all file uploads with a given name -/
def getFilesByName (m : MultipartData) (name : String) : List MultipartPart :=
  m.parts.filter fun p => p.name == name && p.filename.isSome

/-- Get all file uploads -/
def getFiles (m : MultipartData) : List MultipartPart :=
  m.parts.filter fun p => p.filename.isSome

/-- Convert to Params (form fields only, not files) -/
def toParams (m : MultipartData) : List (String × String) :=
  m.parts.filterMap fun p =>
    if p.filename.isNone then
      some (p.name, String.fromUTF8! p.content)
    else
      none

end MultipartData

namespace Multipart

/-- CRLF bytes -/
private def crlf : ByteArray := ⟨#[13, 10]⟩

/-- Double CRLF (header/body separator) -/
private def doubleCrlf : ByteArray := ⟨#[13, 10, 13, 10]⟩

/-- Check if content type is multipart/form-data -/
def isMultipart (contentType : String) : Bool :=
  contentType.startsWith "multipart/form-data"

/-- Extract boundary from Content-Type header -/
def extractBoundary (contentType : String) : Option String :=
  -- Content-Type: multipart/form-data; boundary=----WebKitFormBoundary...
  let parts := contentType.splitOn ";"
  parts.findSome? fun part =>
    let trimmed := part.trim
    if trimmed.startsWith "boundary=" then
      some (trimmed.drop 9 |>.trim.replace "\"" "")
    else
      none

/-- Safe byte access -/
private def getByte (arr : ByteArray) (i : Nat) : UInt8 :=
  if i < arr.size then arr.get! i else 0

/-- Check if bytes match at position -/
private def bytesMatch (haystack needle : ByteArray) (pos : Nat) : Bool :=
  if pos + needle.size > haystack.size then false
  else
    let indices := List.range needle.size
    indices.all fun j => getByte haystack (pos + j) == getByte needle j

/-- Find ByteArray needle in haystack starting at position -/
private def findByteArray (haystack : ByteArray) (needle : ByteArray) (start : Nat := 0) : Option Nat :=
  if needle.size == 0 then some start
  else if start + needle.size > haystack.size then none
  else
    -- Generate list of positions to check
    let positions := List.range (haystack.size - needle.size + 1 - start) |>.map (· + start)
    positions.findSome? fun pos =>
      if bytesMatch haystack needle pos then some pos else none

/-- Extract slice of ByteArray -/
private def sliceByteArray (arr : ByteArray) (start : Nat) (length : Nat) : ByteArray :=
  if start >= arr.size then ByteArray.empty
  else
    let endPos := min (start + length) arr.size
    let bytes := (List.range (endPos - start)).map fun i => getByte arr (start + i)
    ⟨bytes.toArray⟩

/-- Parse a header line like "Content-Disposition: form-data; name=\"field\"; filename=\"test.txt\"" -/
private def parseHeaderLine (line : String) : Option (String × String) :=
  match line.splitOn ":" with
  | [name, value] => some (name.trim.toLower, value.trim)
  | _ => none

/-- Extract quoted value from header parameter like name="value" -/
private def extractQuotedValue (s : String) (param : String) : Option String :=
  let parts := s.splitOn ";"
  parts.findSome? fun part =>
    let trimmed := part.trim
    if trimmed.startsWith (param ++ "=\"") then
      let value := trimmed.drop (param.length + 2)
      if value.endsWith "\"" then
        some (value.dropRight 1)
      else
        some value
    else if trimmed.startsWith (param ++ "=") then
      some (trimmed.drop (param.length + 1))
    else
      none

/-- Parse Content-Disposition header -/
private def parseContentDisposition (value : String) : (String × Option String) :=
  let name := extractQuotedValue value "name" |>.getD ""
  let filename := extractQuotedValue value "filename"
  (name, filename)

/-- Parse a single part (headers + content) -/
private def parsePart (partData : ByteArray) : Option MultipartPart := do
  -- Find the double CRLF that separates headers from content
  let headerEnd ← findByteArray partData doubleCrlf
  let headerBytes := sliceByteArray partData 0 headerEnd
  let headerStr := String.fromUTF8! headerBytes
  let contentStart := headerEnd + 4  -- Skip \r\n\r\n
  let content := sliceByteArray partData contentStart (partData.size - contentStart)

  -- Parse headers
  let headerLines := headerStr.splitOn "\r\n"
  let headers := headerLines.filterMap parseHeaderLine

  -- Get Content-Disposition (required)
  let disposition ← headers.lookup "content-disposition"
  let (name, filename) := parseContentDisposition disposition

  if name.isEmpty then none
  else
    let contentType := headers.lookup "content-type"
    some {
      name := name
      filename := filename
      contentType := contentType
      content := content
    }

/-- Safe byte check (alias for getByte) -/
private def byteAt (arr : ByteArray) (i : Nat) : UInt8 :=
  getByte arr i

/-- Parse complete multipart body -/
def parse (contentType : String) (body : ByteArray) : Option MultipartData := do
  let boundary ← extractBoundary contentType

  -- Boundary marker (end marker is boundaryMarker followed by "--")
  let boundaryMarker := ("--" ++ boundary).toUTF8

  -- Find all parts
  let mut parts : List MultipartPart := []
  let mut pos : Nat := 0

  -- Skip preamble - find first boundary
  match findByteArray body boundaryMarker pos with
  | none => return { parts := [] }
  | some firstBoundary =>
    pos := firstBoundary + boundaryMarker.size

    -- Skip CRLF after boundary
    if pos + 2 <= body.size then
      if byteAt body pos == 13 && byteAt body (pos + 1) == 10 then
        pos := pos + 2

    -- Parse each part
    while pos < body.size do
      -- Find next boundary
      match findByteArray body boundaryMarker pos with
      | none =>
        -- No more boundaries, extract remaining content
        break
      | some nextBoundary =>
        -- Extract part data (everything between current position and next boundary)
        -- Remove trailing CRLF before boundary
        let partEnd := if nextBoundary >= 2 then nextBoundary - 2 else nextBoundary
        let partData := sliceByteArray body pos (partEnd - pos)

        if partData.size > 0 then
          match parsePart partData with
          | some part => parts := parts ++ [part]
          | none => pure ()

        -- Move past boundary
        pos := nextBoundary + boundaryMarker.size

        -- Check if this is the end marker (ends with "--")
        if pos + 2 <= body.size then
          if byteAt body pos == 45 && byteAt body (pos + 1) == 45 then  -- "--"
            break  -- End of multipart

        -- Skip CRLF after boundary
        if pos + 2 <= body.size then
          if byteAt body pos == 13 && byteAt body (pos + 1) == 10 then
            pos := pos + 2

  return { parts := parts }

end Multipart

end Loom
