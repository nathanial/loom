/-
  Loom.Action.Builder - HxConfig builder for composing HTMX attributes

  This module provides a builder pattern for constructing HTMX configurations
  that can be converted to Scribe.Attr lists for use in HTML elements.
-/
import Scribe
import Loom.Action.Types

namespace Loom.Action

open Scribe

/-- Complete HTMX configuration via builder pattern.
    All fields are optional with sensible defaults. -/
structure HxConfig where
  /-- HTTP method for the request -/
  method : HxMethod := .get
  /-- URL path for the request -/
  path : String := ""
  /-- Target element selector -/
  target : Option Target := none
  /-- Swap strategy -/
  swap : Option Swap := none
  /-- Trigger event -/
  trigger : Option TriggerMod := none
  /-- Confirmation dialog message -/
  confirm : Option String := none
  /-- Loading indicator selector -/
  indicator : Option String := none
  /-- Additional elements to include in request -/
  include_ : Option String := none
  /-- JSON values to include -/
  vals : Option String := none
  /-- Additional headers as JSON -/
  headers : Option String := none
  /-- Push URL to browser history -/
  pushUrl : Option String := none
  /-- Replace current URL in history -/
  replaceUrl : Option String := none
  /-- Sync strategy with selector -/
  sync : Option String := none
  /-- Elements to disable during request -/
  disabledElt : Option String := none
  /-- HTMX extensions to use -/
  ext : Option String := none
  /-- Out-of-band select -/
  selectOob : Option String := none
  /-- Out-of-band swap -/
  swapOob : Option String := none
  /-- Params filtering -/
  params : Option Params := none
  /-- Encoding type -/
  encoding : Option String := none
  /-- Enable boost mode -/
  boost : Bool := false
  /-- Preserve element during swap -/
  preserve : Bool := false
  /-- History element marker -/
  historyElt : Bool := false
  /-- Select specific content from response -/
  select : Option String := none
  deriving Repr, Inhabited

namespace HxConfig

/-- Create a GET config -/
def get (path : String) : HxConfig :=
  { method := .get, path }

/-- Create a POST config -/
def post (path : String) : HxConfig :=
  { method := .post, path }

/-- Create a PUT config -/
def put (path : String) : HxConfig :=
  { method := .put, path }

/-- Create a DELETE config -/
def delete (path : String) : HxConfig :=
  { method := .delete, path }

/-- Create a PATCH config -/
def patch (path : String) : HxConfig :=
  { method := .patch, path }

/-- Set the target element -/
def withTarget (cfg : HxConfig) (t : Target) : HxConfig :=
  { cfg with target := some t }

/-- Set target by ID -/
def targetId (cfg : HxConfig) (id : String) : HxConfig :=
  cfg.withTarget (.css s!"#{id}")

/-- Set the swap strategy -/
def withSwap (cfg : HxConfig) (s : Swap) : HxConfig :=
  { cfg with swap := some s }

/-- Set trigger event -/
def withTrigger (cfg : HxConfig) (t : Trigger) : HxConfig :=
  { cfg with trigger := some (TriggerMod.fromTrigger t) }

/-- Set trigger with modifiers -/
def withTriggerMod (cfg : HxConfig) (tm : TriggerMod) : HxConfig :=
  { cfg with trigger := some tm }

/-- Set confirmation dialog -/
def withConfirm (cfg : HxConfig) (msg : String) : HxConfig :=
  { cfg with confirm := some msg }

/-- Set loading indicator -/
def withIndicator (cfg : HxConfig) (selector : String) : HxConfig :=
  { cfg with indicator := some selector }

/-- Set elements to include in request -/
def withInclude (cfg : HxConfig) (selector : String) : HxConfig :=
  { cfg with include_ := some selector }

/-- Set JSON values to include -/
def withVals (cfg : HxConfig) (json : String) : HxConfig :=
  { cfg with vals := some json }

/-- Set additional headers -/
def withHeaders (cfg : HxConfig) (json : String) : HxConfig :=
  { cfg with headers := some json }

/-- Enable URL push to history -/
def withPushUrl (cfg : HxConfig) (url : String := "true") : HxConfig :=
  { cfg with pushUrl := some url }

/-- Enable URL replace in history -/
def withReplaceUrl (cfg : HxConfig) (url : String := "true") : HxConfig :=
  { cfg with replaceUrl := some url }

/-- Set sync strategy -/
def withSync (cfg : HxConfig) (strategy : String) : HxConfig :=
  { cfg with sync := some strategy }

/-- Set elements to disable during request -/
def withDisabledElt (cfg : HxConfig) (selector : String) : HxConfig :=
  { cfg with disabledElt := some selector }

/-- Set HTMX extensions -/
def withExt (cfg : HxConfig) (extensions : String) : HxConfig :=
  { cfg with ext := some extensions }

/-- Set out-of-band select -/
def withSelectOob (cfg : HxConfig) (selector : String) : HxConfig :=
  { cfg with selectOob := some selector }

/-- Set out-of-band swap -/
def withSwapOob (cfg : HxConfig) (value : String) : HxConfig :=
  { cfg with swapOob := some value }

/-- Set params filtering -/
def withParams (cfg : HxConfig) (p : Params) : HxConfig :=
  { cfg with params := some p }

/-- Set encoding type -/
def withEncoding (cfg : HxConfig) (enc : String) : HxConfig :=
  { cfg with encoding := some enc }

/-- Enable boost mode -/
def withBoost (cfg : HxConfig) : HxConfig :=
  { cfg with boost := true }

/-- Enable preserve mode -/
def withPreserve (cfg : HxConfig) : HxConfig :=
  { cfg with preserve := true }

/-- Mark as history element -/
def withHistoryElt (cfg : HxConfig) : HxConfig :=
  { cfg with historyElt := true }

/-- Set content selector -/
def withSelect (cfg : HxConfig) (selector : String) : HxConfig :=
  { cfg with select := some selector }

/-- Convert HxConfig to a list of Scribe.Attr for use in HTML elements -/
def toAttrs (cfg : HxConfig) : List Attr :=
  let attrs : List Attr := []
  -- Method attribute (required)
  let attrs := attrs ++ [⟨cfg.method.toAttrName, cfg.path⟩]
  -- Target
  let attrs := match cfg.target with
    | some t => attrs ++ [⟨"hx-target", t.toString⟩]
    | none => attrs
  -- Swap
  let attrs := match cfg.swap with
    | some s => attrs ++ [⟨"hx-swap", s.toString⟩]
    | none => attrs
  -- Trigger
  let attrs := match cfg.trigger with
    | some t => attrs ++ [⟨"hx-trigger", t.toString⟩]
    | none => attrs
  -- Confirm
  let attrs := match cfg.confirm with
    | some c => attrs ++ [⟨"hx-confirm", c⟩]
    | none => attrs
  -- Indicator
  let attrs := match cfg.indicator with
    | some i => attrs ++ [⟨"hx-indicator", i⟩]
    | none => attrs
  -- Include
  let attrs := match cfg.include_ with
    | some i => attrs ++ [⟨"hx-include", i⟩]
    | none => attrs
  -- Vals
  let attrs := match cfg.vals with
    | some v => attrs ++ [⟨"hx-vals", v⟩]
    | none => attrs
  -- Headers
  let attrs := match cfg.headers with
    | some h => attrs ++ [⟨"hx-headers", h⟩]
    | none => attrs
  -- Push URL
  let attrs := match cfg.pushUrl with
    | some p => attrs ++ [⟨"hx-push-url", p⟩]
    | none => attrs
  -- Replace URL
  let attrs := match cfg.replaceUrl with
    | some r => attrs ++ [⟨"hx-replace-url", r⟩]
    | none => attrs
  -- Sync
  let attrs := match cfg.sync with
    | some s => attrs ++ [⟨"hx-sync", s⟩]
    | none => attrs
  -- Disabled Elt
  let attrs := match cfg.disabledElt with
    | some d => attrs ++ [⟨"hx-disabled-elt", d⟩]
    | none => attrs
  -- Ext
  let attrs := match cfg.ext with
    | some e => attrs ++ [⟨"hx-ext", e⟩]
    | none => attrs
  -- Select OOB
  let attrs := match cfg.selectOob with
    | some s => attrs ++ [⟨"hx-select-oob", s⟩]
    | none => attrs
  -- Swap OOB
  let attrs := match cfg.swapOob with
    | some s => attrs ++ [⟨"hx-swap-oob", s⟩]
    | none => attrs
  -- Params
  let attrs := match cfg.params with
    | some p => attrs ++ [⟨"hx-params", p.toString⟩]
    | none => attrs
  -- Encoding
  let attrs := match cfg.encoding with
    | some e => attrs ++ [⟨"hx-encoding", e⟩]
    | none => attrs
  -- Boost
  let attrs := if cfg.boost then attrs ++ [⟨"hx-boost", "true"⟩] else attrs
  -- Preserve
  let attrs := if cfg.preserve then attrs ++ [⟨"hx-preserve", "true"⟩] else attrs
  -- History Elt
  let attrs := if cfg.historyElt then attrs ++ [⟨"hx-history-elt", ""⟩] else attrs
  -- Select
  let attrs := match cfg.select with
    | some s => attrs ++ [⟨"hx-select", s⟩]
    | none => attrs
  attrs

/-- Merge additional attributes with HxConfig attrs -/
def withAttrs (cfg : HxConfig) (extra : List Attr) : List Attr :=
  cfg.toAttrs ++ extra

end HxConfig

/-- Convenience operators for builder pattern -/
instance : HAppend HxConfig (List Attr) (List Attr) where
  hAppend cfg attrs := cfg.toAttrs ++ attrs

end Loom.Action
