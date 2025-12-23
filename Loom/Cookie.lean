/-
  Loom.Cookie - HTTP Cookie parsing and building
-/

namespace Loom

/-- HTTP Cookie with attributes -/
structure Cookie where
  name : String
  value : String
  path : Option String := some "/"
  maxAge : Option Nat := none
  expires : Option String := none
  domain : Option String := none
  httpOnly : Bool := true
  secure : Bool := false
  sameSite : Option String := some "Lax"
  deriving Repr

namespace Cookie

/-- Convert a hex character to its numeric value -/
private def hexCharToNat (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else none

/-- Convert a hex digit index to a character -/
private def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat ('0'.toNat + n)
  else Char.ofNat ('A'.toNat + n - 10)

/-- URL-decode a string -/
def urlDecode (s : String) : String :=
  let rec loop (chars : List Char) (acc : String) : String :=
    match chars with
    | [] => acc
    | '%' :: h1 :: h2 :: rest =>
      match hexCharToNat h1, hexCharToNat h2 with
      | some n1, some n2 => loop rest (acc.push (Char.ofNat (n1 * 16 + n2)))
      | _, _ => loop rest (acc ++ "%" ++ String.singleton h1 ++ String.singleton h2)
    | '+' :: rest => loop rest (acc.push ' ')
    | c :: rest => loop rest (acc.push c)
  loop s.toList ""

/-- URL-encode a string -/
def urlEncode (s : String) : String :=
  s.foldl (init := "") fun acc c =>
    if c.isAlphanum || c == '-' || c == '_' || c == '.' || c == '~' then
      acc.push c
    else
      let n := c.toNat
      acc ++ "%" ++ String.singleton (hexDigit (n / 16)) ++ String.singleton (hexDigit (n % 16))

/-- Parse a Cookie header value into name-value pairs -/
def parse (headerValue : String) : List (String × String) :=
  headerValue.splitOn ";"
    |>.map String.trim
    |>.filterMap fun part =>
      match part.splitOn "=" with
      | [name, value] => some (name.trim, urlDecode value.trim)
      | [name] => some (name.trim, "")
      | _ => none

/-- Build a Set-Cookie header value -/
def build (c : Cookie) : String :=
  let base := s!"{c.name}={urlEncode c.value}"
  let attrs := [
    c.path.map fun p => s!"; Path={p}",
    c.maxAge.map fun a => s!"; Max-Age={a}",
    c.expires.map fun e => s!"; Expires={e}",
    c.domain.map fun d => s!"; Domain={d}",
    if c.httpOnly then some "; HttpOnly" else none,
    if c.secure then some "; Secure" else none,
    c.sameSite.map fun s => s!"; SameSite={s}"
  ].filterMap id
  base ++ String.join attrs

/-- Create a simple cookie -/
def simple (name value : String) : Cookie :=
  { name, value }

/-- Create a session cookie (no expiry, HttpOnly) -/
def session (name value : String) : Cookie :=
  { name, value, httpOnly := true }

/-- Create a persistent cookie with max age in seconds -/
def persistent (name value : String) (maxAgeSecs : Nat) : Cookie :=
  { name, value, maxAge := some maxAgeSecs, httpOnly := true }

/-- Create a cookie that will be deleted -/
def delete (name : String) : Cookie :=
  { name, value := "", maxAge := some 0 }

end Cookie

end Loom
