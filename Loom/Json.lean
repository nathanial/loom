/-
  Loom.Json - JSON string generation macro

  Provides a `jsonStr!` macro for creating JSON object strings with
  object literal syntax that infers keys from variable names.

  Usage:
    let columnId := 42
    let name := "To Do"
    jsonStr! { columnId, name }
    -- produces: {"columnId": 42, "name": "To Do"}

    jsonStr! { "customKey" : someValue, shorthand }
    -- explicit key syntax mixed with shorthand
-/
import Lean

namespace Loom.Json

/-! ## JSON String Escaping -/

/-- Escape a string for use as a JSON string value.
    Handles quotes, backslashes, and control characters. -/
def escapeString (s : String) : String := Id.run do
  let mut result := ""
  for c in s.toList do
    if c == '"' then result := result ++ "\\\""
    else if c == '\\' then result := result ++ "\\\\"
    else if c == '\n' then result := result ++ "\\n"
    else if c == '\r' then result := result ++ "\\r"
    else if c == '\t' then result := result ++ "\\t"
    else if c.toNat < 32 then
      -- Unicode escape for control characters
      let hex := Nat.toDigits 16 c.toNat
      let padded := String.ofList (List.replicate (4 - hex.length) '0' ++ hex)
      result := result ++ "\\u" ++ padded
    else
      result := result.push c
  return result

/-! ## ToJsonStr Typeclass -/

/-- Typeclass for converting values to their JSON string representation.
    Unlike `ToJson`, this produces a raw string ready for embedding. -/
class ToJsonStr (α : Type) where
  toJsonStr : α → String

instance : ToJsonStr String where
  toJsonStr s := s!"\"{escapeString s}\""

instance : ToJsonStr Nat where
  toJsonStr n := toString n

instance : ToJsonStr Int where
  toJsonStr n := toString n

instance : ToJsonStr Bool where
  toJsonStr b := if b then "true" else "false"

instance : ToJsonStr Float where
  toJsonStr f := toString f

instance : ToJsonStr UInt64 where
  toJsonStr n := toString n.toNat

/-! ## JSON Object Builder -/

/-- Build a JSON object string from an array of key-value pairs.
    Values should already be formatted as JSON (e.g., strings quoted). -/
def buildJsonObject (fields : Array (String × String)) : String :=
  let inner := fields.toList.map (fun (k, v) => s!"\"{k}\": {v}")
    |> String.intercalate ", "
  s!"\{{inner}}"

/-! ## jsonStr! Macro -/

open Lean

/-- Syntax category for JSON fields in jsonStr! macro -/
declare_syntax_cat jsonField

/-- Shorthand field: `varName` (key inferred from identifier) -/
syntax ident : jsonField

/-- Explicit key field: `"key" : expr` -/
syntax str ":" term : jsonField

/-- Create a JSON object string with object literal syntax.

    Examples:
    ```
    let columnId := 42
    jsonStr! { columnId }                    -- {"columnId": 42}
    jsonStr! { "id" : columnId }             -- {"id": 42}
    jsonStr! { columnId, "name" : userName } -- {"columnId": 42, "name": "Alice"}
    ```
-/
scoped syntax "jsonStr! " "{" jsonField,* "}" : term

macro_rules
  | `(jsonStr! { $fields,* }) => do
    let mut pairs : Array (TSyntax `term) := #[]
    for field in fields.getElems do
      match field with
      | `(jsonField| $key:str : $value:term) =>
        -- Explicit key: "key" : value
        pairs := pairs.push (← `(($key, ToJsonStr.toJsonStr $value)))
      | `(jsonField| $id:ident) =>
        -- Shorthand: varName (key = identifier name)
        let keyStr := Syntax.mkStrLit id.getId.toString
        pairs := pairs.push (← `(($keyStr, ToJsonStr.toJsonStr $id)))
      | _ => Macro.throwError "Invalid jsonStr! field syntax"
    `(Loom.Json.buildJsonObject #[$pairs,*])

end Loom.Json
