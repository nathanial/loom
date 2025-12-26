/-
  Loom.Action.Interaction - Interaction type bundling trigger and handler

  An Interaction represents a complete HTMX interaction: the client-side
  trigger configuration and the server-side handler, unified as a single
  typed unit.
-/
import Scribe
import Herald
import Loom.Controller
import Loom.ActionM
import Loom.Action.Types
import Loom.Action.Builder

namespace Loom.Action

open Scribe

/-- Response strategy after handler execution -/
inductive ResponseStrategy where
  /-- Return empty content (useful for delete operations where target is removed) -/
  | remove
  /-- Redirect to a URL -/
  | redirect (url : String)
  /-- Return HTML fragment -/
  | html (content : String)
  /-- Custom response builder -/
  | custom
  deriving Repr, Inhabited

/-- An Interaction bundles HTMX trigger configuration with its server-side handler.
    This creates a single definition site for both the UI trigger and the handler. -/
structure Interaction where
  /-- Unique name for route registration -/
  name : String
  /-- HTTP method -/
  method : HxMethod
  /-- URL path pattern (e.g., "/kanban/card/:id") -/
  pathPattern : String
  /-- HTMX configuration for the trigger element -/
  config : HxConfig
  /-- Response strategy after handler runs -/
  responseStrategy : ResponseStrategy := .remove
  deriving Repr, Inhabited

namespace Interaction

/-- Create a GET interaction -/
def get (name : String) (path : String) : Interaction :=
  { name
  , method := .get
  , pathPattern := path
  , config := HxConfig.get path }

/-- Create a POST interaction -/
def post (name : String) (path : String) : Interaction :=
  { name
  , method := .post
  , pathPattern := path
  , config := HxConfig.post path }

/-- Create a PUT interaction -/
def put (name : String) (path : String) : Interaction :=
  { name
  , method := .put
  , pathPattern := path
  , config := HxConfig.put path }

/-- Create a DELETE interaction -/
def delete (name : String) (path : String) : Interaction :=
  { name
  , method := .delete
  , pathPattern := path
  , config := HxConfig.delete path }

/-- Create a PATCH interaction -/
def patch (name : String) (path : String) : Interaction :=
  { name
  , method := .patch
  , pathPattern := path
  , config := HxConfig.patch path }

/-- Set the target element -/
def target (i : Interaction) (t : Target) : Interaction :=
  { i with config := i.config.withTarget t }

/-- Set target by element ID -/
def targetId (i : Interaction) (id : String) : Interaction :=
  i.target (.css s!"#{id}")

/-- Set the swap strategy -/
def swap (i : Interaction) (s : Swap) : Interaction :=
  { i with config := i.config.withSwap s }

/-- Set confirmation dialog -/
def confirm (i : Interaction) (msg : String) : Interaction :=
  { i with config := i.config.withConfirm msg }

/-- Set trigger event -/
def trigger (i : Interaction) (t : Trigger) : Interaction :=
  { i with config := i.config.withTrigger t }

/-- Set trigger with modifiers -/
def triggerMod (i : Interaction) (tm : TriggerMod) : Interaction :=
  { i with config := i.config.withTriggerMod tm }

/-- Set loading indicator -/
def indicator (i : Interaction) (selector : String) : Interaction :=
  { i with config := i.config.withIndicator selector }

/-- Enable URL push to history -/
def pushUrl (i : Interaction) (url : String := "true") : Interaction :=
  { i with config := i.config.withPushUrl url }

/-- Enable URL replace in history -/
def replaceUrl (i : Interaction) (url : String := "true") : Interaction :=
  { i with config := i.config.withReplaceUrl url }

/-- Set elements to include in request -/
def includeElt (i : Interaction) (selector : String) : Interaction :=
  { i with config := i.config.withInclude selector }

/-- Set JSON values to include -/
def vals (i : Interaction) (json : String) : Interaction :=
  { i with config := i.config.withVals json }

/-- Set additional headers -/
def headers (i : Interaction) (json : String) : Interaction :=
  { i with config := i.config.withHeaders json }

/-- Set sync strategy -/
def sync (i : Interaction) (strategy : String) : Interaction :=
  { i with config := i.config.withSync strategy }

/-- Set elements to disable during request -/
def disabledElt (i : Interaction) (selector : String) : Interaction :=
  { i with config := i.config.withDisabledElt selector }

/-- Set HTMX extensions -/
def ext (i : Interaction) (extensions : String) : Interaction :=
  { i with config := i.config.withExt extensions }

/-- Set out-of-band select -/
def selectOob (i : Interaction) (selector : String) : Interaction :=
  { i with config := i.config.withSelectOob selector }

/-- Set out-of-band swap -/
def swapOob (i : Interaction) (value : String) : Interaction :=
  { i with config := i.config.withSwapOob value }

/-- Set params filtering -/
def params (i : Interaction) (p : Params) : Interaction :=
  { i with config := i.config.withParams p }

/-- Enable boost mode -/
def boost (i : Interaction) : Interaction :=
  { i with config := i.config.withBoost }

/-- Enable preserve mode -/
def preserve (i : Interaction) : Interaction :=
  { i with config := i.config.withPreserve }

/-- Set content selector -/
def select (i : Interaction) (selector : String) : Interaction :=
  { i with config := i.config.withSelect selector }

/-- Set response strategy -/
def respond (i : Interaction) (r : ResponseStrategy) : Interaction :=
  { i with responseStrategy := r }

/-- Get HTMX attributes for use in an element -/
def attrs (i : Interaction) : List Attr :=
  i.config.toAttrs

/-- Get HTMX attributes merged with additional attributes -/
def attrsPlus (i : Interaction) (extra : List Attr) : List Attr :=
  i.config.toAttrs ++ extra

end Interaction

end Loom.Action
