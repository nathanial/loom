/-
  Loom.Json - JSON string generation macro (re-exports from Staple)

  This module re-exports `jsonStr!` and related utilities from Staple.Json
  for backwards compatibility. New code should import Staple.Json directly.
-/
import Staple.Json

namespace Loom.Json

-- Re-export everything from Staple.Json
export Staple.Json (escapeString ToJsonStr buildJsonObject)

end Loom.Json
