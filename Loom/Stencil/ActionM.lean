/-
  Loom.Stencil.ActionM - Template rendering helpers for ActionM
-/
import Stencil
import Loom.ActionM
import Loom.SSE
import Loom.Stencil.Config
import Loom.Stencil.Manager
import Loom.Stencil.Convert

namespace Loom.Stencil

/-- Render error type -/
inductive RenderError where
  | noStencilManager : RenderError
  | templateNotFound : String → RenderError
  | layoutNotFound : String → RenderError
  | renderError : String → RenderError
  deriving Repr

instance : ToString RenderError where
  toString
    | .noStencilManager => "Stencil template engine not configured"
    | .templateNotFound name => s!"Template not found: {name}"
    | .layoutNotFound name => s!"Layout not found: {name}"
    | .renderError msg => s!"Render error: {msg}"

namespace ActionM

/-- Get the stencil manager from the context.
    Returns None if Stencil is not configured for this app. -/
def getStencilManager : Loom.ActionM (Option (IO.Ref Manager)) := do
  let ctx ← get
  pure ctx.stencilManager

/-- Check and perform hot reload if needed.
    Publishes SSE event to "hot-reload" topic when templates change. -/
private def maybeHotReload (managerRef : IO.Ref Manager) : IO Unit := do
  let manager ← managerRef.get
  if ← manager.shouldCheckReload then
    let (updated, changed) ← manager.checkAndReload
    managerRef.set updated
    if changed then
      Loom.SSE.publishEvent "hot-reload" "reload" ""

/-- Render a template by name with user data.
    The template receives:
    - All user-provided data
    - `params` - URL/form parameters
    - `session` - Session data
    - `flash` - Flash messages
    - `csrfToken` - CSRF token -/
def render (name : String) (data : Stencil.Value := .null) : Loom.ActionM Herald.Core.Response := do
  let ctx ← get
  match ctx.stencilManager with
  | none => throw (IO.userError (toString RenderError.noStencilManager))
  | some managerRef =>
    -- Hot reload check
    maybeHotReload managerRef

    let manager ← managerRef.get

    -- Find template
    match manager.getTemplate name with
    | none => throw (IO.userError (toString (RenderError.templateNotFound name)))
    | some tmpl =>
      -- Build Stencil context with merged data
      let stencilCtx := buildStencilContext ctx data manager.getPartials

      -- Render template
      match Stencil.renderString tmpl stencilCtx with
      | .ok htmlStr =>
        -- Check for default layout
        match manager.config.defaultLayout with
        | none => Loom.ActionM.html htmlStr
        | some layoutName =>
          match manager.getLayout layoutName with
          | none => Loom.ActionM.html htmlStr  -- No layout found, just return content
          | some layoutTmpl =>
            -- Render layout with content as "content" block
            let layoutCtx := stencilCtx.addPartial "content" tmpl
            match Stencil.renderString layoutTmpl layoutCtx with
            | .ok layoutHtml => Loom.ActionM.html layoutHtml
            | .error e => throw (IO.userError (toString (RenderError.renderError (toString e))))
      | .error e => throw (IO.userError (toString (RenderError.renderError (toString e))))

/-- Render a template with an explicit layout.
    The layout template should use `{{> content}}` to include the main content. -/
def renderWithLayout (layout : String) (name : String) (data : Stencil.Value := .null)
    : Loom.ActionM Herald.Core.Response := do
  let ctx ← get
  match ctx.stencilManager with
  | none => throw (IO.userError (toString RenderError.noStencilManager))
  | some managerRef =>
    maybeHotReload managerRef
    let manager ← managerRef.get

    -- Find template
    match manager.getTemplate name with
    | none => throw (IO.userError (toString (RenderError.templateNotFound name)))
    | some tmpl =>
      -- Find layout
      match manager.getLayout layout with
      | none => throw (IO.userError (toString (RenderError.layoutNotFound layout)))
      | some layoutTmpl =>
        -- Build context
        let stencilCtx := buildStencilContext ctx data manager.getPartials

        -- First render the content template
        match Stencil.renderString tmpl stencilCtx with
        | .error e => throw (IO.userError (toString (RenderError.renderError (toString e))))
        | .ok contentHtml =>
          -- Pass rendered content as a string value
          -- Layout should use {{{content}}} (triple braces) for raw/unescaped output
          let layoutCtx := stencilCtx.mergeData (.object #[("content", .string contentHtml)])
          match Stencil.renderString layoutTmpl layoutCtx with
          | .ok layoutHtml => Loom.ActionM.html layoutHtml
          | .error e => throw (IO.userError (toString (RenderError.renderError (toString e))))

/-- Render an inline template string.
    Useful for simple one-off templates without creating a file. -/
def renderInline (template : String) (data : Stencil.Value := .null)
    : Loom.ActionM Herald.Core.Response := do
  let ctx ← get
  -- Parse inline template
  match Stencil.parse template with
  | .error e => throw (IO.userError (toString (RenderError.renderError (toString e))))
  | .ok tmpl =>
    -- For inline templates, we use empty partials (since we can't access IO here)
    -- In practice, inline templates rarely need partials
    let partials : Std.HashMap String Stencil.Template := {}

    let stencilCtx := buildStencilContext ctx data partials
    match Stencil.renderString tmpl stencilCtx with
    | .ok htmlStr => Loom.ActionM.html htmlStr
    | .error e => throw (IO.userError (toString (RenderError.renderError (toString e))))

/-- Render a partial template directly (useful for HTMX responses) -/
def renderPartial (name : String) (data : Stencil.Value := .null)
    : Loom.ActionM Herald.Core.Response := do
  let ctx ← get
  match ctx.stencilManager with
  | none => throw (IO.userError (toString RenderError.noStencilManager))
  | some managerRef =>
    maybeHotReload managerRef
    let manager ← managerRef.get

    match manager.getPartial name with
    | none => throw (IO.userError (toString (RenderError.templateNotFound s!"partial:{name}")))
    | some tmpl =>
      let stencilCtx := buildStencilContext ctx data manager.getPartials
      match Stencil.renderString tmpl stencilCtx with
      | .ok htmlStr => Loom.ActionM.html htmlStr
      | .error e => throw (IO.userError (toString (RenderError.renderError (toString e))))

end ActionM

end Loom.Stencil
