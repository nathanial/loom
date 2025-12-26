/-
  Loom.Action - HTMX Action/Interaction system

  This module provides a high-level abstraction for HTMX interactions,
  bundling client-side trigger configurations with server-side handlers.

  ## Overview

  The Action module provides:
  - `HxMethod`, `Swap`, `Target`, `Trigger` - HTMX primitive types
  - `HxConfig` - Builder pattern for HTMX attributes
  - `Interaction` - Bundles trigger config for non-parameterized interactions
  - `InteractionFactory` - Creates parameterized interactions

  ## Example Usage

  ### Simple Interaction (no parameters)
  ```lean
  def refreshList : Interaction :=
    Interaction.get "refreshList" "/items"
      |>.target (.css "#item-list")
      |>.swap .innerHTML

  -- In view:
  refreshList.button "Refresh" [class_ "btn btn-secondary"]
  ```

  ### Parameterized Interaction
  ```lean
  def deleteItem : InteractionFactory Nat :=
    InteractionFactory.delete "deleteItem" "/items/:id"
      (pathFor := fun id => s!"/items/{id}")
      |>.target (.css s!"#item-{id}")  -- Note: id available via closure
      |>.swap .outerHTML
      |>.confirm "Delete this item?"

  -- In view:
  deleteItem.button item.id "Delete" [class_ "btn btn-danger"]
  ```

  ### Using HxConfig directly
  ```lean
  -- For one-off configurations without defining an Interaction
  let cfg := HxConfig.post "/api/submit"
    |>.withTarget (.css "#result")
    |>.withSwap .innerHTML
    |>.withIndicator "#loading"

  button (cfg ++ [class_ "btn"]) (text "Submit")
  ```
-/

import Loom.Action.Types
import Loom.Action.Builder
import Loom.Action.Interaction
import Loom.Action.Parameterized
import Loom.Action.Scribe
import Loom.Action.Macro

namespace Loom

-- Re-export core types at Loom namespace level
export Action (
  HxMethod
  Swap
  Target
  Trigger
  TriggerMod
  Sync
  Params
  HxConfig
  ResponseStrategy
  Interaction
  InteractionFactory
)

end Loom
