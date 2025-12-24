/-
  Loom.IncludeStr - Compile-time file embedding

  Usage: `include_str% "path/to/file"`

  The file path is relative to the Lean source file.
-/
import Lean

namespace Loom

open Lean Elab Term

/-- Read a file at compile time and embed its contents as a string literal.
    Path is relative to the source file containing the `include_str%` call. -/
scoped elab "include_str% " path:str : term => do
  let ctx ← readThe Core.Context
  let srcPath := ctx.fileName
  let srcDir := System.FilePath.parent srcPath |>.getD ""
  let filePath := srcDir / path.getString
  let contents ← IO.FS.readFile filePath
  return mkStrLit contents

end Loom
