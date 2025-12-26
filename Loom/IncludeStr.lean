/-
  Loom.IncludeStr - Compile-time file embedding

  Re-exports `include_str%` from Staple for backward compatibility.

  Usage: `include_str% "path/to/file"`

  The file path is relative to the Lean source file.
-/
import Staple

-- The macro is available globally via `import Staple`
-- This module exists for backward compatibility with existing Loom imports
