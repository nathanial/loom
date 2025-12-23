/-
  Loom.Form - Form parsing and CSRF protection
-/
import Loom.Cookie
import Loom.Session

namespace Loom

/-- Parsed form/query parameters -/
abbrev Params := List (String × String)

namespace Params

/-- Empty params -/
def empty : Params := []

/-- Get a parameter value -/
def get (p : Params) (key : String) : Option String :=
  p.lookup key

/-- Get a parameter with default -/
def getD (p : Params) (key : String) (default : String) : String :=
  p.get key |>.getD default

/-- Get all values for a key (for multi-select) -/
def getAll (p : Params) (key : String) : List String :=
  p.filterMap fun (k, v) => if k == key then some v else none

/-- Check if a parameter exists -/
def has (p : Params) (key : String) : Bool :=
  p.any (·.1 == key)

/-- Merge two param lists (second takes precedence) -/
def merge (p1 p2 : Params) : Params :=
  let keys2 := p2.map (·.1)
  p1.filter (fun (k, _) => !keys2.contains k) ++ p2

end Params

namespace Form

/-- Parse URL-encoded form data (application/x-www-form-urlencoded) -/
def parseUrlEncoded (body : String) : Params :=
  if body.isEmpty then []
  else
    body.splitOn "&"
      |>.filterMap fun part =>
        match part.splitOn "=" with
        | [k, v] => some (Cookie.urlDecode k, Cookie.urlDecode v)
        | [k] => some (Cookie.urlDecode k, "")
        | _ => none

/-- Parse query string from a URL path -/
def parseQueryString (path : String) : Params :=
  match path.splitOn "?" with
  | [_, query] => parseUrlEncoded query
  | _ => []

/-- Extract path without query string -/
def pathWithoutQuery (path : String) : String :=
  match path.splitOn "?" with
  | [p, _] => p
  | _ => path

/-- Simple hash for CSRF tokens -/
private def csrfHash (data : ByteArray) : String :=
  let init1 : UInt64 := 0x12345678
  let init2 : UInt64 := 0x87654321
  let h1 : UInt64 := data.foldl (init := init1) fun (h : UInt64) (b : UInt8) =>
    ((h <<< 5) + h) ^^^ b.toUInt64
  let h2 : UInt64 := data.foldl (init := init2) fun (h : UInt64) (b : UInt8) =>
    ((h <<< 7) + h) ^^^ b.toUInt64
  let digits1 := h1.toNat.toDigits 16
  let digits2 := h2.toNat.toDigits 16
  String.ofList digits1 ++ String.ofList digits2

/-- Generate a CSRF token tied to the session -/
def generateCsrfToken (secret : ByteArray) (session : Session) : String :=
  -- Use session data as entropy source
  let sessionData := session.data.foldl (init := "") fun acc (k, v) => acc ++ k ++ v
  let data := secret ++ sessionData.toUTF8 ++ "csrf".toUTF8
  csrfHash data

/-- Validate a CSRF token -/
def validateCsrfToken (token : String) (secret : ByteArray) (session : Session) : Bool :=
  let expected := generateCsrfToken secret session
  token == expected

/-- Check if request method requires CSRF validation -/
def methodRequiresCsrf (method : String) : Bool :=
  method == "POST" || method == "PUT" || method == "PATCH" || method == "DELETE"

end Form

end Loom
