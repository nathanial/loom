/-
  Loom.Action.Macro - The `interaction` macro for declarative HTMX interactions

  This macro allows defining parameterized interactions with a clean syntax:

  ```lean
  -- Trigger-only (no handler)
  interaction deleteCard "/kanban/card/:id" DELETE (id : Nat)
    |>.target (.css s!"#card-{id}")
    |>.swap .outerHTML
    |>.confirm "Delete this card?"

  -- Use in view:
  deleteCard.button cardId "Delete"
  ```

  With an optional handler, the interaction also registers a route:

  ```lean
  interaction deleteCard "/kanban/card/:id" delete (id : Nat)
    |>.swap .outerHTML
    |>.confirm "Delete this card?"
  handler do
    let ctx ← getCtx
    if !isLoggedIn ctx then return ← redirect "/login"
    -- ... handler code
    html ""
  ```

  The macro generates an InteractionFactory with the correct path interpolation,
  and optionally a handler + route registration.
-/
import Lean
import Loom.Action.Types
import Loom.Action.Builder
import Loom.Action.Parameterized
import Loom.Page

namespace Loom.Action

open Lean Elab Command Meta Term

/-! ## Interaction Registry -/

/-- Information about a registered interaction for route generation -/
structure InteractionInfo where
  name : Name
  path : String
  method : String
  factoryName : Name
  paramNames : Array String
  paramTypes : Array String
  deriving Repr, Inhabited

/-! ## Syntax -/

/-- Interaction parameter syntax: `(name : Type)` -/
syntax interactionParam := "(" ident ":" ident ")"

/-- Interaction modifier for chained configuration -/
syntax interactionMod := "|>." ident (term)?

/-- Interaction syntax: `interaction name "path" METHOD (params)* mods* (handler do body)?` -/
syntax (name := interactionCmd)
  "interaction" ident str ident interactionParam* interactionMod* ("handler" "do" doSeq)? : command

/-! ## Elaborator -/

/-- Parse an interaction parameter -/
private def parseInteractionParam (stx : Syntax) : CommandElabM (String × String) := do
  match stx with
  | `(interactionParam| ($name:ident : $ty:ident)) =>
    return (name.getId.toString, ty.getId.toString)
  | _ => throwError "Invalid interaction parameter syntax"

/-- Generate the pathFor function body -/
private def genPathForBody (pathPattern : String) (paramNames : Array String) : String :=
  -- Convert "/kanban/card/:id" to s!"/kanban/card/{id}"
  let parts := pathPattern.splitOn "/"
  let converted := parts.map fun part =>
    if part.startsWith ":" then
      let paramName := part.drop 1
      "{" ++ paramName ++ "}"
    else
      part
  let interpPath := "/".intercalate converted
  if paramNames.isEmpty then
    s!"\"{pathPattern}\""
  else
    s!"s!\"{interpPath}\""

/-- Parse a modifier and return the method name and argument -/
private def parseModifier (stx : Syntax) : CommandElabM (String × Option Syntax) := do
  match stx with
  | `(interactionMod| |>. $methodName:ident $[$arg:term]?) =>
    return (methodName.getId.toString, arg)
  | _ => throwError "Invalid interaction modifier syntax"

@[command_elab interactionCmd]
def elabInteraction : CommandElab := fun stx => do
  match stx with
  | `(command| interaction $name:ident $path:str $method:ident $params:interactionParam* $mods:interactionMod* $[handler do $body:doSeq]?) =>
    let interactionName := name.getId
    let pathStr := path.getString
    let methodStr := method.getId.toString.toUpper

    -- Parse parameters
    let parsedParams ← params.mapM parseInteractionParam
    let paramNames := parsedParams.map Prod.fst
    let paramTypes := parsedParams.map Prod.snd

    -- Get current namespace
    let currNs ← getCurrNamespace
    let fullName := currNs ++ interactionName
    let infoNameStr := interactionName.toString ++ "_interactionInfo"
    let handlerNameStr := interactionName.toString ++ "_handler"
    let pageInfoNameStr := interactionName.toString ++ "_pageInfo"

    -- Build the method constructor
    let methodCtor := match methodStr with
      | "GET" => "get"
      | "POST" => "post"
      | "PUT" => "put"
      | "DELETE" => "delete"
      | "PATCH" => "patch"
      | _ => "get"

    -- Generate pathFor body
    let pathForBody := genPathForBody pathStr paramNames

    -- Parse modifiers
    let parsedMods ← mods.mapM parseModifier

    -- Build modifier chain
    let modChain := parsedMods.foldl (init := "") fun acc (methodName, argOpt) =>
      match argOpt with
      | some arg => acc ++ s!"\n    |>.{methodName} {arg.reprint.getD ""}"
      | none => acc ++ s!"\n    |>.{methodName}"

    -- Generate the definition based on number of parameters
    let defCode := if parsedParams.isEmpty then
      -- No parameters: generate an Interaction
      s!"
def {interactionName} : Loom.Action.Interaction :=
  Loom.Action.Interaction.{methodCtor} \"{interactionName}\" \"{pathStr}\"{modChain}
"
    else
      -- With parameters: generate an InteractionFactory
      let paramTypeSig := if parsedParams.size == 1 then
        parsedParams[0]!.2
      else
        -- For multiple params, we use a tuple
        let types := parsedParams.map Prod.snd
        "(" ++ " × ".intercalate types.toList ++ ")"

      -- Generate pathFor lambda
      let pathForLambda := if parsedParams.size == 1 then
        s!"fun {paramNames[0]!} => {pathForBody}"
      else
        -- For tuples, destructure
        let destructure := "(" ++ ", ".intercalate paramNames.toList ++ ")"
        s!"fun {destructure} => {pathForBody}"

      s!"
def {interactionName} : Loom.Action.InteractionFactory {paramTypeSig} :=
  Loom.Action.InteractionFactory.{methodCtor} \"{interactionName}\" \"{pathStr}\"
    (pathFor := {pathForLambda}){modChain}
"

    -- Parse and elaborate the definition
    let env ← getEnv
    let opts ← getOptions
    let fileName ← getFileName
    let inputCtx := Parser.mkInputContext defCode fileName
    let parserState : Parser.ModuleParserState := {}
    let commandState := Command.mkState env {} opts
    let s ← IO.processCommands inputCtx parserState commandState

    for msg in s.commandState.messages.toList do
      if msg.severity == MessageSeverity.error then
        logError s!"Error in interaction generation: {← msg.data.toString}"
        logInfo s!"Generated code:\n{defCode}"

    setEnv s.commandState.env

    -- Generate InteractionInfo for route registration
    let paramNamesStr := "#[" ++ ", ".intercalate (paramNames.map (s!"\"{·}\"")).toList ++ "]"
    let paramTypesStr := "#[" ++ ", ".intercalate (paramTypes.map (s!"\"{·}\"")).toList ++ "]"

    let infoCode := s!"
def {infoNameStr} : Loom.Action.InteractionInfo := \{
  name := `{fullName}
  path := \"{pathStr}\"
  method := \"{methodStr}\"
  factoryName := `{fullName}
  paramNames := {paramNamesStr}
  paramTypes := {paramTypesStr}
}
"
    let inputCtx := Parser.mkInputContext infoCode fileName
    let parserState : Parser.ModuleParserState := {}
    let commandState := Command.mkState s.commandState.env {} opts
    let s ← IO.processCommands inputCtx parserState commandState

    for msg in s.commandState.messages.toList do
      if msg.severity == MessageSeverity.error then
        logError s!"Error in interactionInfo generation: {← msg.data.toString}"

    setEnv s.commandState.env

    -- If handler body is provided, generate handler and pageInfo
    if let some handlerBody := body then
      let handlerIdent := mkIdent (Name.mkSimple handlerNameStr)

      -- Build extractor let bindings for parameters (same as page macro)
      let mut extractorCmds : Array (TSyntax `Lean.Parser.Term.doSeqItem) := #[]
      for p in parsedParams do
        let paramNameIdent := mkIdent (Name.mkSimple p.1)
        let paramNameStr := Syntax.mkStrLit p.1
        let extractorItem ← if p.2 == "String" then
          `(Lean.Parser.Term.doSeqItem| let $paramNameIdent := ctx.paramD $paramNameStr "")
        else
          `(Lean.Parser.Term.doSeqItem| let $paramNameIdent := (ctx.paramD $paramNameStr "0").toNat?.getD 0)
        extractorCmds := extractorCmds.push extractorItem

      -- Generate handler definition
      let handlerDef ← `(command|
        def $handlerIdent : Loom.Action := fun ctx => do
          $extractorCmds*
          let (resp, ctx') ← (do $handlerBody : Loom.ActionM Herald.Core.Response).run ctx
          pure (resp, ctx')
      )
      elabCommand handlerDef

      -- Generate pageInfo for route registration (reuse page infrastructure)
      let fullHandlerName := currNs ++ (Name.mkSimple handlerNameStr)
      let paramsArray := parsedParams.map fun p =>
        s!"\{ name := \"{p.1}\", type := \"{p.2}\" }"
      let paramsStr := "#[" ++ ", ".intercalate paramsArray.toList ++ "]"

      let pageInfoCode := s!"
def {pageInfoNameStr} : Loom.Page.PageInfo := \{
  name := `{fullName}
  path := \"{pathStr}\"
  method := \"{methodStr}\"
  handlerName := `{fullHandlerName}
  params := {paramsStr}
}
"
      let env ← getEnv
      let inputCtx := Parser.mkInputContext pageInfoCode fileName
      let parserState : Parser.ModuleParserState := {}
      let commandState := Command.mkState env {} opts
      let s ← IO.processCommands inputCtx parserState commandState

      for msg in s.commandState.messages.toList do
        if msg.severity == MessageSeverity.error then
          logError s!"Error in pageInfo generation: {← msg.data.toString}"
          logInfo s!"Generated code:\n{pageInfoCode}"

      setEnv s.commandState.env

  | _ => throwUnsupportedSyntax

/-! ## Route Generation -/

/-- Find all InteractionInfo constants in the environment -/
def findInteractionInfos : MetaM (Array InteractionInfo) := do
  let env ← getEnv
  let mut infos : Array InteractionInfo := #[]

  for (name, _) in env.constants.map₁.toList do
    if name.isInternal then continue
    let nameStr := name.toString
    if !nameStr.endsWith "_interactionInfo" then continue
    try
      let val ← unsafe evalConst InteractionInfo name
      infos := infos.push val
    catch _ =>
      continue

  return infos

/-- Generate a handler registration line for an interaction -/
private def genInteractionRegisterLine (currNs : Name) (info : InteractionInfo) : String :=
  let localName := info.name.replacePrefix currNs .anonymous
  let methodLower := info.method.toLower
  s!"    |>.{methodLower} \"{info.path}\" \"{localName}\" {info.factoryName}_handler"

/-- Command to generate interaction routes (extend #generate_pages) -/
syntax (name := generateInteractions) "#generate_interactions" : command

@[command_elab generateInteractions]
def elabGenerateInteractions : CommandElab := fun _stx => do
  let infos ← liftTermElabM findInteractionInfos

  if infos.isEmpty then
    logWarning "No interactions registered. Use 'interaction name path METHOD (params)* mods*' to define interactions."
    return

  let currNs ← getCurrNamespace

  -- Log what we found
  for info in infos do
    logInfo s!"Found interaction: {info.name} - {info.method} {info.path}"

end Loom.Action
