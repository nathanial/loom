/-
  Loom.Routing.Macros - Route definition macro

  Provides the `define_routes` macro for defining routes with minimal boilerplate.
  A single definition generates the Route type, path function, pattern function,
  and all necessary instances.

  ## Usage

  ```lean
  define_routes Route
    | home => "/"
        GET Actions.Home.index
    | login => "/login"
        GET Actions.Auth.loginForm
        POST Actions.Auth.login
    | kanbanGetColumn (id : Nat) => "/kanban/column/{id}"
        GET (withId Actions.Kanban.getColumnPartial)
  end
  ```

  This generates:
  - `inductive Route` with all constructors
  - `Route.path : Route → String` (for views, with interpolation)
  - `Route.pattern : Route → String` (for registration, with :param syntax)
  - `Route.routeName : Route → String` (snake_case name for registration)
  - `Route.registerAll : Loom.App → Loom.App` (registers all handlers)
  - `instance : ToString Route`
  - `instance : Scribe.HasPath Route`
-/
import Lean
import Scribe

/-! ## Syntax Definition -/

/-- A route parameter: `(name : Type)` -/
syntax routeParam := "(" ident ":" term ")"

/-- A route handler: `METHOD action` (e.g., `GET Actions.Home.index`) -/
syntax routeHandler := ident term

/-- A single route case: `| name (params)* => "path" handlers*` -/
syntax routeCase := "|" ident routeParam* "=>" str routeHandler*

/-- Main syntax: `define_routes Name ... end` -/
syntax (name := defineRoutes)
  "define_routes" ident routeCase* "end" : command

namespace Loom.Routing.Macros

open Lean Elab Command Meta

/-! ## Helper Functions -/

/-- Convert camelCase to snake_case -/
def toSnakeCase (s : String) : String :=
  let chars := s.toList
  let result := chars.foldl (init := []) fun acc c =>
    if c.isUpper then
      if acc.isEmpty then
        [c.toLower]
      else
        '_' :: c.toLower :: acc
    else
      c :: acc
  String.mk result.reverse

/-- Check if string contains a placeholder -/
private def hasPlaceholder (s : String) : Bool :=
  s.toList.any (· == '{')

/-- Extract parameter names from a path template like "/kanban/column/{id}" -/
private def extractPathParams (path : String) : List String :=
  let parts := path.splitOn "{"
  parts.tail.filterMap fun part =>
    match part.splitOn "}" with
    | name :: _ => some name
    | [] => none

/-- Convert path template to pattern for registration.
    "/kanban/column/{id}" becomes "/kanban/column/:id" -/
private def pathToPattern (path : String) : String :=
  let params := extractPathParams path
  params.foldl (fun acc param => acc.replace s!"\{{param}}" s!":{param}") path

/-! ## Route Case Parsing -/

/-- Parsed handler data -/
structure ParsedHandler where
  method : String   -- "GET", "POST", etc.
  action : String   -- action term as string
  deriving Inhabited

/-- Parsed route case data -/
structure ParsedRouteCase where
  name : String
  params : Array (String × String)  -- (paramName, paramType as string)
  pathTemplate : String
  handlers : Array ParsedHandler    -- method + action bindings
  deriving Inhabited

/-- Parse a routeParam syntax node -/
private def parseRouteParam (stx : Syntax) : CommandElabM (String × String) := do
  match stx with
  | `(routeParam| ($name:ident : $ty:term)) =>
    return (name.getId.toString, ty.raw.reprint.getD "Nat")
  | _ => throwError "Invalid route parameter syntax"

/-- Parse a routeHandler syntax node -/
private def parseRouteHandler (stx : Syntax) : CommandElabM ParsedHandler := do
  match stx with
  | `(routeHandler| $method:ident $action:term) =>
    return {
      method := method.getId.toString
      action := action.raw.reprint.getD ""
    }
  | _ => throwError "Invalid route handler syntax"

/-- Parse a single route case -/
private def parseRouteCase (stx : Syntax) : CommandElabM ParsedRouteCase := do
  match stx with
  | `(routeCase| | $name:ident $params:routeParam* => $path:str $handlers:routeHandler*) =>
    let parsedParams ← params.mapM parseRouteParam
    let parsedHandlers ← handlers.mapM parseRouteHandler
    return {
      name := name.getId.toString
      params := parsedParams
      pathTemplate := path.getString
      handlers := parsedHandlers
    }
  | _ => throwError "Invalid route case syntax"

/-! ## Code String Generation -/

/-- Generate constructor string for the inductive type -/
private def genCtorString (rc : ParsedRouteCase) : String :=
  if rc.params.isEmpty then
    s!"  | {rc.name}"
  else
    let paramStr := rc.params.foldl (init := "") fun acc (pn, pt) =>
      acc ++ s!" ({pn} : {pt})"
    s!"  | {rc.name}{paramStr}"

/-- Generate path match arm string -/
private def genPathArmString (rc : ParsedRouteCase) : String :=
  let dotCtor := s!".{rc.name}"
  if rc.params.isEmpty then
    s!"  | {dotCtor} => \"{rc.pathTemplate}\""
  else
    let paramList := rc.params.foldl (init := "") fun acc (pn, _) => acc ++ s!" {pn}"
    if hasPlaceholder rc.pathTemplate then
      s!"  | {dotCtor}{paramList} => s!\"{rc.pathTemplate}\""
    else
      s!"  | {dotCtor}{paramList} => \"{rc.pathTemplate}\""

/-- Generate pattern match arm string -/
private def genPatternArmString (rc : ParsedRouteCase) : String :=
  let dotCtor := s!".{rc.name}"
  let patternStr := pathToPattern rc.pathTemplate
  if rc.params.isEmpty then
    s!"  | {dotCtor} => \"{patternStr}\""
  else
    let wildcards := rc.params.foldl (init := "") fun acc _ => acc ++ " _"
    s!"  | {dotCtor}{wildcards} => \"{patternStr}\""

/-- Generate routeName match arm string -/
private def genNameArmString (rc : ParsedRouteCase) : String :=
  let dotCtor := s!".{rc.name}"
  let snakeName := toSnakeCase rc.name
  if rc.params.isEmpty then
    s!"  | {dotCtor} => \"{snakeName}\""
  else
    let wildcards := rc.params.foldl (init := "") fun acc _ => acc ++ " _"
    s!"  | {dotCtor}{wildcards} => \"{snakeName}\""

/-- Generate dummy arguments for parametrized routes (0 for Nat, "" for String) -/
private def genDummyArgs (params : Array (String × String)) : String :=
  params.foldl (init := "") fun acc (_, ty) =>
    let dummy := if ty.trim == "String" then "\"\"" else "0"
    acc ++ s!" {dummy}"

/-- Generate a single registration line for a handler -/
private def genRegisterLine (rc : ParsedRouteCase) (h : ParsedHandler) : String :=
  let methodLower := h.method.toLower
  let routeExpr := if rc.params.isEmpty then
    rc.name
  else
    s!"({rc.name}{genDummyArgs rc.params})"
  s!"    |>.{methodLower}' {routeExpr} {h.action}"

/-- Generate all registration lines from parsed cases -/
private def genRegisterAllLines (cases : Array ParsedRouteCase) : List String :=
  cases.toList.flatMap fun rc =>
    rc.handlers.toList.map fun h => genRegisterLine rc h

/-! ## Main Elaborator -/

@[command_elab defineRoutes]
def elabDefineRoutes : CommandElab := fun stx => do
  match stx with
  | `(command| define_routes $typeName:ident $cases:routeCase* end) =>
    let typeNameStr := typeName.getId.toString

    -- Get current namespace to wrap generated code
    let currNs ← getCurrNamespace
    let nsPrefix := if currNs.isAnonymous then "" else s!"namespace {currNs}\n\n"
    let nsSuffix := if currNs.isAnonymous then "" else s!"\nend {currNs}\n"

    -- Parse all route cases
    let parsedCases ← cases.mapM parseRouteCase

    -- Build constructor strings
    let ctorStrs := parsedCases.map genCtorString
    let ctorsCode := String.intercalate "\n" ctorStrs.toList

    -- Build path arm strings
    let pathArmStrs := parsedCases.map genPathArmString
    let pathArmsCode := String.intercalate "\n" pathArmStrs.toList

    -- Build pattern arm strings
    let patternArmStrs := parsedCases.map genPatternArmString
    let patternArmsCode := String.intercalate "\n" patternArmStrs.toList

    -- Build name arm strings
    let nameArmStrs := parsedCases.map genNameArmString
    let nameArmsCode := String.intercalate "\n" nameArmStrs.toList

    -- Build registerAll lines (only if there are handlers)
    let registerLines := genRegisterAllLines parsedCases
    let hasHandlers := !registerLines.isEmpty
    let registerAllCode := if hasHandlers then
      let lines := String.intercalate "\n" registerLines
      s!"
def registerAll (app : Loom.App) : Loom.App :=
  app
{lines}
"
    else ""

    -- Generate the complete code (wrapped in current namespace)
    let code := s!"{nsPrefix}inductive {typeNameStr} where
{ctorsCode}
  deriving Repr

namespace {typeNameStr}

def path : {typeNameStr} → String
{pathArmsCode}

def pattern : {typeNameStr} → String
{patternArmsCode}

def routeName : {typeNameStr} → String
{nameArmsCode}

instance : ToString {typeNameStr} where
  toString := path

instance : Scribe.HasPath {typeNameStr} where
  path := path

instance : Loom.App.HasRouteInfo {typeNameStr} where
  pattern := pattern
  routeName := routeName
{registerAllCode}
end {typeNameStr}{nsSuffix}"

    -- Use Lean.Elab.Frontend to parse and run the commands
    let env ← getEnv
    let opts ← getOptions
    let fileName ← getFileName

    -- Create a simple parser context and parse/elaborate
    let inputCtx := Parser.mkInputContext code fileName
    let parserState : Parser.ModuleParserState := {}
    let commandState := Command.mkState env {} opts
    let s ← IO.processCommands inputCtx parserState commandState

    -- Update the environment with the elaborated commands
    setEnv s.commandState.env

  | _ => throwUnsupportedSyntax

end Loom.Routing.Macros
