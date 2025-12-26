/-
  Loom.Action.Parameterized - InteractionFactory for parameterized interactions

  This module provides InteractionFactory, which creates Interactions that
  depend on runtime parameters (e.g., a delete button that needs an item ID).
-/
import Scribe
import Loom.Action.Types
import Loom.Action.Builder
import Loom.Action.Interaction

namespace Loom.Action

open Scribe

/-- A factory for creating parameterized Interactions.
    Given parameters, produces a concrete Interaction with the correct path.

    Example: A delete button that needs an item ID:
    ```lean
    def deleteCard : InteractionFactory Nat :=
      InteractionFactory.delete "deleteCard" "/kanban/card/{id}"
        (pathFor := fun id => s!"/kanban/card/{id}")
        |>.target (.css "#card-{id}")
        |>.swap .outerHTML
        |>.confirm "Delete this card?"
    ```
-/
structure InteractionFactory (Params : Type) where
  /-- Base name for the interaction -/
  name : String
  /-- HTTP method -/
  method : HxMethod
  /-- Path pattern with placeholders -/
  pathPattern : String
  /-- Function to generate concrete path from params -/
  pathFor : Params → String
  /-- Base HTMX config (path will be set per instantiation) -/
  baseConfig : HxConfig
  /-- Response strategy -/
  responseStrategy : ResponseStrategy := .remove
  deriving Inhabited

namespace InteractionFactory

/-- Create a GET interaction factory -/
def get (name : String) (pathPattern : String) (pathFor : P → String) : InteractionFactory P :=
  { name
  , method := .get
  , pathPattern
  , pathFor
  , baseConfig := { method := .get } }

/-- Create a POST interaction factory -/
def post (name : String) (pathPattern : String) (pathFor : P → String) : InteractionFactory P :=
  { name
  , method := .post
  , pathPattern
  , pathFor
  , baseConfig := { method := .post } }

/-- Create a PUT interaction factory -/
def put (name : String) (pathPattern : String) (pathFor : P → String) : InteractionFactory P :=
  { name
  , method := .put
  , pathPattern
  , pathFor
  , baseConfig := { method := .put } }

/-- Create a DELETE interaction factory -/
def delete (name : String) (pathPattern : String) (pathFor : P → String) : InteractionFactory P :=
  { name
  , method := .delete
  , pathPattern
  , pathFor
  , baseConfig := { method := .delete } }

/-- Create a PATCH interaction factory -/
def patch (name : String) (pathPattern : String) (pathFor : P → String) : InteractionFactory P :=
  { name
  , method := .patch
  , pathPattern
  , pathFor
  , baseConfig := { method := .patch } }

/-- Create a concrete Interaction for given parameters -/
def instantiate (f : InteractionFactory P) (params : P) : Interaction :=
  let path := f.pathFor params
  { name := f.name
  , method := f.method
  , pathPattern := f.pathPattern
  , config := { f.baseConfig with path := path }
  , responseStrategy := f.responseStrategy }

/-- Get HTMX attributes for given params -/
def attrsFor (f : InteractionFactory P) (params : P) : List Attr :=
  (f.instantiate params).attrs

/-- Get HTMX attributes merged with additional attributes -/
def attrsForPlus (f : InteractionFactory P) (params : P) (extra : List Attr) : List Attr :=
  (f.instantiate params).attrsPlus extra

/-- Set the target element (with access to params) -/
def targetFor (f : InteractionFactory P) (targetFn : P → Target) : InteractionFactory P :=
  { f with baseConfig := { f.baseConfig with target := none } }

/-- Set a static target element -/
def target (f : InteractionFactory P) (t : Target) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withTarget t }

/-- Set target by static ID -/
def targetId (f : InteractionFactory P) (id : String) : InteractionFactory P :=
  f.target (.css s!"#{id}")

/-- Set the swap strategy -/
def swap (f : InteractionFactory P) (s : Swap) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withSwap s }

/-- Set confirmation dialog -/
def confirm (f : InteractionFactory P) (msg : String) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withConfirm msg }

/-- Set trigger event -/
def trigger (f : InteractionFactory P) (t : Trigger) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withTrigger t }

/-- Set loading indicator -/
def indicator (f : InteractionFactory P) (selector : String) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withIndicator selector }

/-- Enable URL push to history -/
def pushUrl (f : InteractionFactory P) (url : String := "true") : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withPushUrl url }

/-- Enable URL replace in history -/
def replaceUrl (f : InteractionFactory P) (url : String := "true") : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withReplaceUrl url }

/-- Set elements to include in request -/
def includeElt (f : InteractionFactory P) (selector : String) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withInclude selector }

/-- Set JSON values to include -/
def vals (f : InteractionFactory P) (json : String) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withVals json }

/-- Set additional headers -/
def headers (f : InteractionFactory P) (json : String) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withHeaders json }

/-- Set sync strategy -/
def sync (f : InteractionFactory P) (strategy : String) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withSync strategy }

/-- Set elements to disable during request -/
def disabledElt (f : InteractionFactory P) (selector : String) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withDisabledElt selector }

/-- Set HTMX extensions -/
def ext (f : InteractionFactory P) (extensions : String) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withExt extensions }

/-- Set out-of-band select -/
def selectOob (f : InteractionFactory P) (selector : String) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withSelectOob selector }

/-- Set out-of-band swap -/
def swapOob (f : InteractionFactory P) (value : String) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withSwapOob value }

/-- Set params filtering -/
def params (f : InteractionFactory P) (p : Params) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withParams p }

/-- Enable boost mode -/
def boost (f : InteractionFactory P) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withBoost }

/-- Enable preserve mode -/
def preserve (f : InteractionFactory P) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withPreserve }

/-- Set content selector -/
def select (f : InteractionFactory P) (selector : String) : InteractionFactory P :=
  { f with baseConfig := f.baseConfig.withSelect selector }

/-- Set response strategy -/
def respond (f : InteractionFactory P) (r : ResponseStrategy) : InteractionFactory P :=
  { f with responseStrategy := r }

end InteractionFactory

end Loom.Action
