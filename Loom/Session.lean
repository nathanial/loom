/-
  Loom.Session - Cookie-based session management with signing
-/
import Loom.Cookie

namespace Loom

/-- Simple hash function for signing (FNV-1a variant) -/
private def fnvHash (data : ByteArray) : UInt64 :=
  let fnvPrime : UInt64 := 0x00000100000001B3
  let fnvOffset : UInt64 := 0xcbf29ce484222325
  data.foldl (init := fnvOffset) fun hash byte =>
    (hash ^^^ byte.toUInt64) * fnvPrime

/-- Convert UInt64 to hex string -/
private def toHexString (n : UInt64) : String :=
  let digits := Nat.toDigits 16 n.toNat
  String.ofList digits

/-- Sign data with a secret key -/
private def sign (data : String) (secret : ByteArray) : String :=
  let combined := secret ++ data.toUTF8
  let hash := fnvHash combined
  -- Mix in the hash bytes for a second round
  let hashBytes := ByteArray.mk #[hash.toUInt8, (hash >>> 8).toUInt8, (hash >>> 16).toUInt8, (hash >>> 24).toUInt8]
  let hash2 := fnvHash (combined ++ hashBytes)
  s!"{toHexString hash}{toHexString hash2}"

/-- Verify signature -/
private def verifySignature (data signature : String) (secret : ByteArray) : Bool :=
  sign data secret == signature

/-- Session data stored in a signed cookie -/
structure Session where
  data : List (String × String)
  modified : Bool := false
  deriving Repr

namespace Session

/-- Empty session -/
def empty : Session := { data := [], modified := false }

/-- Get a value from the session -/
def get (s : Session) (key : String) : Option String :=
  s.data.lookup key

/-- Set a value in the session -/
def set (s : Session) (key value : String) : Session :=
  let newData := (key, value) :: s.data.filter (·.1 != key)
  { data := newData, modified := true }

/-- Delete a key from the session -/
def delete (s : Session) (key : String) : Session :=
  { data := s.data.filter (·.1 != key), modified := true }

/-- Clear all session data -/
def clear (s : Session) : Session :=
  { data := [], modified := true }

/-- Check if session has a key -/
def has (s : Session) (key : String) : Bool :=
  s.data.any (·.1 == key)

/-- Check if session is empty -/
def isEmpty (s : Session) : Bool :=
  s.data.isEmpty

/-- Encode session data to a string (key=value pairs separated by &) -/
private def encodeData (data : List (String × String)) : String :=
  data.map (fun (k, v) => s!"{Cookie.urlEncode k}={Cookie.urlEncode v}")
    |> String.intercalate "&"

/-- Decode session data from a string -/
private def decodeData (s : String) : List (String × String) :=
  if s.isEmpty then []
  else
    s.splitOn "&"
      |>.filterMap fun part =>
        match part.splitOn "=" with
        | [k, v] => some (Cookie.urlDecode k, Cookie.urlDecode v)
        | _ => none

/-- Encode session to a signed cookie value -/
def encode (s : Session) (secret : ByteArray) : String :=
  let data := encodeData s.data
  let sig := sign data secret
  let encoded := Cookie.urlEncode data
  s!"{encoded}.{sig}"

/-- Find the last occurrence of a character in a string -/
private def findLastIndex (s : String) (c : Char) : Option Nat :=
  let chars := s.toList
  let len := chars.length
  chars.reverse.findIdx? (· == c) |>.map (len - · - 1)

/-- Decode and verify a session from a cookie value -/
def decode (cookie : String) (secret : ByteArray) : Option Session :=
  -- Split on the LAST dot (signature is always at the end)
  match findLastIndex cookie '.' with
  | none => none
  | some dotIdx =>
    let encodedData := cookie.take dotIdx
    let signature := cookie.drop (dotIdx + 1)
    let data := Cookie.urlDecode encodedData
    if verifySignature data signature secret then
      some { data := decodeData data, modified := false }
    else
      none

/-- Get session ID (creates one if not present) -/
def getId (s : Session) : IO (String × Session) := do
  match s.get "_id" with
  | some id => pure (id, s)
  | none =>
    -- Generate a simple session ID based on timestamp
    let now ← IO.monoNanosNow
    let id := s!"sess_{now % 1000000000000}"
    pure (id, s.set "_id" id)

/-- Build a session cookie -/
def toCookie (s : Session) (secret : ByteArray) (name : String := "loom_session") : Cookie :=
  { name := name
  , value := s.encode secret
  , httpOnly := true
  , sameSite := some "Lax"
  , path := some "/"
  }

end Session

end Loom
