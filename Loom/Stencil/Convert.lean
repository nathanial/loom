/-
  Loom.Stencil.Convert - Convert Loom types to Stencil Values
-/
import Stencil
import Loom.Controller

namespace Loom.Stencil

/-- Convert a list of key-value pairs to a Stencil object -/
def pairsToValue (pairs : List (String × String)) : Stencil.Value :=
  .object (pairs.map (fun (k, v) => (k, Stencil.Value.string v))).toArray

/-- Convert Loom params to Stencil Value -/
def paramsToValue (params : Loom.Params) : Stencil.Value :=
  pairsToValue params

/-- Convert Loom session to Stencil Value -/
def sessionToValue (session : Loom.Session) : Stencil.Value :=
  pairsToValue session.data

/-- Convert Loom flash messages to Stencil Value -/
def flashToValue (flash : Loom.Flash) : Stencil.Value :=
  pairsToValue flash.current

/-- Convert request info to Stencil Value -/
def requestToValue (req : Citadel.ServerRequest) : Stencil.Value :=
  Stencil.Value.object #[
    ("path", .string req.path),
    ("method", .string (toString req.method)),
    ("fullPath", .string req.fullPath)
  ]

/-- Convert Loom.Context to Stencil.Value with automatic context data.
    The resulting value contains:
    - `params` - URL/form parameters
    - `session` - Session data
    - `flash` - Flash messages
    - `csrfToken` - CSRF token for forms
    - `request` - Request metadata (path, method, etc.)
-/
def contextToValue (ctx : Loom.Context) : Stencil.Value :=
  Stencil.Value.object #[
    ("params", paramsToValue ctx.params),
    ("session", sessionToValue ctx.session),
    ("flash", flashToValue ctx.flash),
    ("csrfToken", .string ctx.csrfToken),
    ("request", requestToValue ctx.request)
  ]

/-- Merge user-provided data with context data.
    User data takes precedence over context data. -/
def mergeWithContext (ctx : Loom.Context) (userData : Stencil.Value) : Stencil.Value :=
  let ctxValue := contextToValue ctx
  match ctxValue, userData with
  | .object ctxFields, .object userFields =>
    -- User fields override context fields
    let merged := ctxFields.foldl (init := userFields) fun acc (k, v) =>
      if userFields.any (·.1 == k) then acc
      else acc.push (k, v)
    .object merged
  | .object ctxFields, .null =>
    .object ctxFields
  | _, _ =>
    -- If user data isn't an object, wrap it as "data"
    match ctxValue with
    | .object fields => .object (fields.push ("data", userData))
    | _ => userData

/-- Build a Stencil.Context from Loom.Context with user data and partials -/
def buildStencilContext (loomCtx : Loom.Context) (userData : Stencil.Value)
    (partials : Std.HashMap String Stencil.Template) : Stencil.Context :=
  let mergedData := mergeWithContext loomCtx userData
  let baseCtx := Stencil.contextFromValue mergedData
  -- Register all partials
  partials.fold (init := baseCtx) fun ctx name tmpl =>
    ctx.addPartial name tmpl

end Loom.Stencil
