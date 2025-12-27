/-
  Loom.Page - Unified page definitions (route + logic + view)

  The `page` macro combines route, action, and view into one definition:

  ```lean
  -- Simple page (no parameters)
  page home "/" GET do
    if !(← isLoggedIn) then return ← redirect "/login"
    html (layout ctx do h1 [] (text "Welcome!"))

  -- Parameterized page
  page kanbanCard "/kanban/card/:id" GET (id : Nat) do
    -- id is extracted from URL and available as Nat
    let card ← getCard id
    html (renderCard card)
  ```

  Use `#generate_pages` at the end to create the Route type and registration function.
-/
import Lean
import Scribe
import Loom.Controller
import Loom.ActionM
import Loom.App

namespace Loom.Page

open Lean Elab Command Meta Term

/-! ## Page Registry -/

/-- A route parameter with name and type -/
structure RouteParam where
  name : String
  type : String  -- "Nat" or "String"
  deriving Repr, Inhabited

/-- Information about a registered page -/
structure PageInfo where
  name : Name
  path : String
  method : String  -- "GET", "POST", etc.
  handlerName : Name
  params : Array RouteParam
  middleware : Array String := #[]  -- Middleware term names as strings
  deriving Repr, Inhabited

/-! ## Syntax -/

/-- Route parameter syntax: `(name : Type)` -/
syntax pageParam := "(" ident ":" ident ")"

/-- Middleware list syntax: `[mw1, mw2, ...]` -/
syntax middlewareList := "[" term,* "]"

/-- Page syntax: `page name "path" METHOD [middleware]? (params)* do body` -/
syntax (name := pageCmd) "page" ident str ident (middlewareList)? pageParam* "do" doSeq : command

/-- View syntax: `view name "path" [middleware]? (params)* do body` - GET route that renders HTML -/
syntax (name := viewCmd) "view" ident str (middlewareList)? pageParam* "do" doSeq : command

/-- Action syntax: `action name "path" METHOD [middleware]? (params)* do body` - mutation route -/
syntax (name := actionCmd) "action" ident str ident (middlewareList)? pageParam* "do" doSeq : command

/-! ## Macros -/

/-- View expands to page with GET method (with middleware) -/
macro_rules
  | `(command| view $name:ident $path:str $mw:middlewareList $params:pageParam* do $body:doSeq) =>
    let getIdent := Lean.mkIdent `GET
    `(command| page $name $path $getIdent $mw $params* do $body)

/-- View expands to page with GET method (without middleware) -/
macro_rules
  | `(command| view $name:ident $path:str $params:pageParam* do $body:doSeq) =>
    let getIdent := Lean.mkIdent `GET
    `(command| page $name $path $getIdent [] $params* do $body)

/-- Action expands to page with specified method (with middleware) -/
macro_rules
  | `(command| action $name:ident $path:str $method:ident $mw:middlewareList $params:pageParam* do $body:doSeq) =>
    `(command| page $name $path $method $mw $params* do $body)

/-- Action expands to page with specified method (without middleware) -/
macro_rules
  | `(command| action $name:ident $path:str $method:ident $params:pageParam* do $body:doSeq) =>
    `(command| page $name $path $method [] $params* do $body)

/-! ## Elaborator -/

/-- Parse a pageParam syntax node -/
private def parsePageParam (stx : Syntax) : CommandElabM RouteParam := do
  match stx with
  | `(pageParam| ($name:ident : $ty:ident)) =>
    return { name := name.getId.toString, type := ty.getId.toString }
  | _ => throwError "Invalid page parameter syntax"

/-- Extract middleware terms from middlewareList syntax -/
private def parseMiddlewareList (stx : Option (TSyntax `Loom.Page.middlewareList)) : Array String :=
  match stx with
  | none => #[]
  | some mwList =>
    -- middlewareList is `"[" term,* "]"`, extract the terms
    let args := mwList.raw[1]  -- The Syntax.SepArray of terms
    args.getSepArgs.map (·.reprint.getD "")

/-- Core elaboration logic for page command -/
private def elabPageCore (name : TSyntax `ident) (path : TSyntax `str) (method : TSyntax `ident)
    (mwList : Option (TSyntax `Loom.Page.middlewareList)) (params : Array (TSyntax `Loom.Page.pageParam))
    (body : TSyntax `Lean.Parser.Term.doSeq) : CommandElabM Unit := do
  let pageName := name.getId
  let pathStr := path.getString
  let methodStr := method.getId.toString.toUpper
  let middleware := parseMiddlewareList mwList

  -- Parse parameters
  let parsedParams ← params.mapM parsePageParam

  -- Get current namespace
  let currNs ← getCurrNamespace
  let fullName := currNs ++ pageName
  let handlerNameStr := pageName.toString ++ "_handler"
  let infoNameStr := pageName.toString ++ "_pageInfo"
  let handlerIdent := mkIdent (Name.mkSimple handlerNameStr)

  -- For parameterized routes, generate let bindings that extract params
  -- The page body can use these names directly
  if parsedParams.isEmpty then
    -- Simple handler (no params)
    let handlerDef ← `(command|
      def $handlerIdent : Loom.Action := fun ctx => do
        let (resp, ctx') ← (do $body : Loom.ActionM Herald.Core.Response).run ctx
        pure (resp, ctx')
    )
    elabCommand handlerDef
  else
    -- For parameterized routes, we generate extractors first then the body
    -- Build extractor let bindings
    let mut extractorCmds : Array (TSyntax `Lean.Parser.Term.doSeqItem) := #[]
    for p in parsedParams do
      let paramNameIdent := mkIdent (Name.mkSimple p.name)
      let paramNameStr := Syntax.mkStrLit p.name
      let extractorItem ← if p.type == "String" then
        `(Lean.Parser.Term.doSeqItem| let $paramNameIdent := ctx.paramD $paramNameStr "")
      else
        `(Lean.Parser.Term.doSeqItem| let $paramNameIdent := (ctx.paramD $paramNameStr "0").toNat?.getD 0)
      extractorCmds := extractorCmds.push extractorItem

    -- Generate handler with extractors and body
    -- Use the extractors as a sequence of doSeqItems, then include body
    let handlerDef ← `(command|
      def $handlerIdent : Loom.Action := fun ctx => do
        $extractorCmds*
        let (resp, ctx') ← (do $body : Loom.ActionM Herald.Core.Response).run ctx
        pure (resp, ctx')
    )
    elabCommand handlerDef

  -- Create pageInfo definition with parameters and middleware
  let fullHandlerName := currNs ++ (Name.mkSimple handlerNameStr)
  let paramsArray := parsedParams.map fun p =>
    s!"\{ name := \"{p.name}\", type := \"{p.type}\" }"
  let paramsStr := "#[" ++ ", ".intercalate paramsArray.toList ++ "]"
  let middlewareArray := middleware.map fun m => s!"\"{m}\""
  let middlewareStr := "#[" ++ ", ".intercalate middlewareArray.toList ++ "]"

  let infoCode := s!"
def {infoNameStr} : Loom.Page.PageInfo := \{
  name := `{fullName}
  path := \"{pathStr}\"
  method := \"{methodStr}\"
  handlerName := `{fullHandlerName}
  params := {paramsStr}
  middleware := {middlewareStr}
}
"
  let env ← getEnv
  let opts ← getOptions
  let fileName ← getFileName
  let inputCtx := Parser.mkInputContext infoCode fileName
  let parserState : Parser.ModuleParserState := {}
  let commandState := Command.mkState env {} opts
  let s ← IO.processCommands inputCtx parserState commandState

  for msg in s.commandState.messages.toList do
    if msg.severity == MessageSeverity.error then
      logError s!"Error in pageInfo generation: {← msg.data.toString}"

  setEnv s.commandState.env

@[command_elab pageCmd]
def elabPage : CommandElab := fun stx => do
  match stx with
  | `(command| page $name:ident $path:str $method:ident $mw:middlewareList $params:pageParam* do $body:doSeq) =>
    elabPageCore name path method (some mw) params body
  | `(command| page $name:ident $path:str $method:ident $params:pageParam* do $body:doSeq) =>
    elabPageCore name path method none params body
  | _ => throwUnsupportedSyntax

/-! ## Route Generation -/

/-- Convert path to pattern (keeps :param syntax) -/
def pathToPattern (path : String) : String := path

/-- Convert path to interpolated string ("/user/:id" → "/user/{id}") -/
def pathToInterpolation (path : String) : String :=
  let parts := path.splitOn "/"
  let converted := parts.map fun part =>
    if part.startsWith ":" then "{" ++ (part.drop 1) ++ "}" else part
  "/".intercalate converted

/-- Convert camelCase to snake_case -/
def toSnakeCase (s : String) : String :=
  let chars := s.toList
  let result := chars.foldl (init := []) fun acc c =>
    if c.isUpper then
      if acc.isEmpty then [c.toLower]
      else c.toLower :: '_' :: acc  -- Insert _ before the uppercase letter
    else c :: acc
  String.ofList result.reverse

/-- Find all PageInfo constants in the environment -/
def findPageInfos : MetaM (Array PageInfo) := do
  let env ← getEnv
  let mut pages : Array PageInfo := #[]

  for (name, _) in env.constants.map₁.toList do
    if name.isInternal then continue
    let nameStr := name.toString
    if !nameStr.endsWith "_pageInfo" then continue
    try
      let val ← unsafe evalConst PageInfo name
      pages := pages.push val
    catch _ =>
      continue

  return pages

/-- Generate constructor string for Route type -/
private def genCtorString (currNs : Name) (p : PageInfo) : String :=
  let localName := p.name.replacePrefix currNs .anonymous
  if p.params.isEmpty then
    s!"  | {localName}"
  else
    let paramStr := p.params.foldl (init := "") fun acc param =>
      acc ++ s!" ({param.name} : {param.type})"
    s!"  | {localName}{paramStr}"

/-- Generate path function arm -/
private def genPathArm (currNs : Name) (p : PageInfo) : String :=
  let localName := p.name.replacePrefix currNs .anonymous
  let interpPath := pathToInterpolation p.path
  if p.params.isEmpty then
    s!"  | .{localName} => \"{p.path}\""
  else
    let paramList := p.params.foldl (init := "") fun acc param =>
      acc ++ s!" {param.name}"
    s!"  | .{localName}{paramList} => s!\"{interpPath}\""

/-- Generate pattern function arm -/
private def genPatternArm (currNs : Name) (p : PageInfo) : String :=
  let localName := p.name.replacePrefix currNs .anonymous
  if p.params.isEmpty then
    s!"  | .{localName} => \"{p.path}\""
  else
    let wildcards := p.params.foldl (init := "") fun acc _ => acc ++ " _"
    s!"  | .{localName}{wildcards} => \"{p.path}\""

/-- Generate routeName function arm -/
private def genNameArm (currNs : Name) (p : PageInfo) : String :=
  let localName := p.name.replacePrefix currNs .anonymous
  let snakeName := toSnakeCase localName.toString
  if p.params.isEmpty then
    s!"  | .{localName} => \"{snakeName}\""
  else
    let wildcards := p.params.foldl (init := "") fun acc _ => acc ++ " _"
    s!"  | .{localName}{wildcards} => \"{snakeName}\""

/-- Generate registration line -/
private def genRegisterLine (currNs : Name) (p : PageInfo) : String :=
  let localName := p.name.replacePrefix currNs .anonymous
  let localHandlerName := p.handlerName.replacePrefix currNs .anonymous
  -- Ensure handler name is properly formatted (use full path if needed)
  let handlerStr := if localHandlerName.isAnonymous then p.handlerName.toString else localHandlerName.toString
  let methodLower := p.method.toLower
  let snakeName := toSnakeCase localName.toString
  -- Include middleware list (empty list if no middleware)
  let mwStr := if p.middleware.isEmpty then "[]"
               else "[" ++ ", ".intercalate p.middleware.toList ++ "]"
  s!"    |>.{methodLower} \"{p.path}\" \"{snakeName}\" {mwStr} {handlerStr}"

/-- Generate routes command: creates Route type and registerPages function -/
syntax (name := generatePages) "#generate_pages" : command

@[command_elab generatePages]
def elabGeneratePages : CommandElab := fun _stx => do
  let pages ← liftTermElabM findPageInfos

  if pages.isEmpty then
    logWarning "No pages registered. Use 'page name path METHOD do body' to define pages."
    return

  let currNs ← getCurrNamespace

  -- Build Route constructors
  let ctors := pages.map (genCtorString currNs)
  let ctorsCode := String.intercalate "\n" ctors.toList

  -- Build path function
  let pathArms := pages.map (genPathArm currNs)
  let pathCode := String.intercalate "\n" pathArms.toList

  -- Build pattern function
  let patternArms := pages.map (genPatternArm currNs)
  let patternCode := String.intercalate "\n" patternArms.toList

  -- Build routeName function
  let nameArms := pages.map (genNameArm currNs)
  let nameCode := String.intercalate "\n" nameArms.toList

  -- Build registerPages function
  let regLines := pages.map (genRegisterLine currNs)
  let regCode := String.intercalate "\n" regLines.toList

  -- Generate full code
  let code := s!"
inductive Route where
{ctorsCode}
  deriving Repr

namespace Route

def path : Route → String
{pathCode}

def pattern : Route → String
{patternCode}

def routeName : Route → String
{nameCode}

instance : ToString Route where
  toString := path

instance : Scribe.HasPath Route where
  path := path

instance : Loom.App.HasRouteInfo Route where
  pattern := pattern
  routeName := routeName

end Route

def registerPages (app : Loom.App) : Loom.App :=
  app
{regCode}
"

  -- Wrap code in namespace
  let nsStr := currNs.toString
  let fullCode := s!"namespace {nsStr}\n{code}\nend {nsStr}"

  -- Parse and elaborate
  let env ← getEnv
  let opts ← getOptions
  let fileName ← getFileName
  let inputCtx := Parser.mkInputContext fullCode fileName
  let parserState : Parser.ModuleParserState := {}
  let commandState := Command.mkState env {} opts
  let s ← IO.processCommands inputCtx parserState commandState

  for msg in s.commandState.messages.toList do
    if msg.severity == MessageSeverity.error then
      logError s!"Error processing generated code: {← msg.data.toString}"

  setEnv s.commandState.env

end Loom.Page
