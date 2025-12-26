/-
  Loom.Action.Scribe - HtmlM integration for Interactions

  This module provides convenience functions for rendering Interaction
  triggers within Scribe's HtmlM builder monad.
-/
import Scribe
import Loom.Action.Types
import Loom.Action.Builder
import Loom.Action.Interaction
import Loom.Action.Parameterized

namespace Loom.Action

open Scribe

/-! ## Interaction Element Helpers -/

/-- Render a button that triggers an Interaction -/
def Interaction.button (i : Interaction) (label : String)
    (extraAttrs : List Attr := []) : HtmlM Unit :=
  Scribe.button (i.attrsPlus extraAttrs) (HtmlM.text label)

/-- Render a button with custom content that triggers an Interaction -/
def Interaction.buttonM (i : Interaction) (extraAttrs : List Attr := [])
    (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.button (i.attrsPlus extraAttrs) content

/-- Render a link that triggers an Interaction -/
def Interaction.link (i : Interaction) (label : String)
    (extraAttrs : List Attr := []) : HtmlM Unit :=
  Scribe.a (i.attrsPlus extraAttrs) (HtmlM.text label)

/-- Render a link with custom content that triggers an Interaction -/
def Interaction.linkM (i : Interaction) (extraAttrs : List Attr := [])
    (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.a (i.attrsPlus extraAttrs) content

/-- Render a form that triggers an Interaction -/
def Interaction.formM (i : Interaction) (extraAttrs : List Attr := [])
    (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.form (i.attrsPlus extraAttrs) content

/-- Render a div that triggers an Interaction -/
def Interaction.divM (i : Interaction) (extraAttrs : List Attr := [])
    (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.div (i.attrsPlus extraAttrs) content

/-- Apply Interaction attributes to any element -/
def Interaction.element (i : Interaction) (tag : String)
    (extraAttrs : List Attr := []) (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.element tag (i.attrsPlus extraAttrs) content

/-! ## InteractionFactory Element Helpers -/

/-- Render a button that triggers a parameterized Interaction -/
def InteractionFactory.button (f : InteractionFactory P) (params : P)
    (label : String) (extraAttrs : List Attr := []) : HtmlM Unit :=
  Scribe.button (f.attrsForPlus params extraAttrs) (HtmlM.text label)

/-- Render a button with custom content for a parameterized Interaction -/
def InteractionFactory.buttonM (f : InteractionFactory P) (params : P)
    (extraAttrs : List Attr := []) (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.button (f.attrsForPlus params extraAttrs) content

/-- Render a link that triggers a parameterized Interaction -/
def InteractionFactory.link (f : InteractionFactory P) (params : P)
    (label : String) (extraAttrs : List Attr := []) : HtmlM Unit :=
  Scribe.a (f.attrsForPlus params extraAttrs) (HtmlM.text label)

/-- Render a link with custom content for a parameterized Interaction -/
def InteractionFactory.linkM (f : InteractionFactory P) (params : P)
    (extraAttrs : List Attr := []) (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.a (f.attrsForPlus params extraAttrs) content

/-- Render a form that triggers a parameterized Interaction -/
def InteractionFactory.formM (f : InteractionFactory P) (params : P)
    (extraAttrs : List Attr := []) (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.form (f.attrsForPlus params extraAttrs) content

/-- Render a div that triggers a parameterized Interaction -/
def InteractionFactory.divM (f : InteractionFactory P) (params : P)
    (extraAttrs : List Attr := []) (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.div (f.attrsForPlus params extraAttrs) content

/-- Apply parameterized Interaction attributes to any element -/
def InteractionFactory.element (f : InteractionFactory P) (params : P)
    (tag : String) (extraAttrs : List Attr := [])
    (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.element tag (f.attrsForPlus params extraAttrs) content

/-! ## HxConfig Element Helpers -/

/-- Render a button with HxConfig attributes -/
def HxConfig.button (cfg : HxConfig) (label : String)
    (extraAttrs : List Attr := []) : HtmlM Unit :=
  Scribe.button (cfg.withAttrs extraAttrs) (HtmlM.text label)

/-- Render a button with custom content using HxConfig -/
def HxConfig.buttonM (cfg : HxConfig) (extraAttrs : List Attr := [])
    (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.button (cfg.withAttrs extraAttrs) content

/-- Render a link with HxConfig attributes -/
def HxConfig.link (cfg : HxConfig) (label : String)
    (extraAttrs : List Attr := []) : HtmlM Unit :=
  Scribe.a (cfg.withAttrs extraAttrs) (HtmlM.text label)

/-- Render a form with HxConfig attributes -/
def HxConfig.formM (cfg : HxConfig) (extraAttrs : List Attr := [])
    (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.form (cfg.withAttrs extraAttrs) content

/-- Apply HxConfig attributes to any element -/
def HxConfig.element (cfg : HxConfig) (tag : String)
    (extraAttrs : List Attr := []) (content : HtmlM Unit) : HtmlM Unit :=
  Scribe.element tag (cfg.withAttrs extraAttrs) content

end Loom.Action
