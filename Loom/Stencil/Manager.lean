/-
  Loom.Stencil.Manager - Template manager with discovery and hot reload
-/
import Stencil
import Loom.Stencil.Config
import Std.Data.HashMap

-- Alias to avoid namespace conflicts (Loom.Stencil shadows the Stencil module)
abbrev StencilEngine := Stencil.Engine
abbrev StencilTemplate := Stencil.Template

namespace Loom.Stencil

/-- Check if a substring is contained in a string -/
private def containsSubstr (s sub : String) : Bool :=
  (s.splitOn sub).length > 1

/-- A cached template entry with metadata for hot reload -/
structure TemplateEntry where
  /-- The parsed template -/
  template : StencilTemplate
  /-- Original file path -/
  path : System.FilePath
  /-- File modification time (for hot reload) -/
  mtime : UInt64
  deriving Inhabited

/-- Template manager holding all discovered templates -/
structure Manager where
  /-- Configuration -/
  config : Config
  /-- Stencil engine with parse cache -/
  engine : StencilEngine
  /-- Regular templates (name -> entry) -/
  templates : Std.HashMap String TemplateEntry
  /-- Partial templates (name -> entry with mtime for hot reload) -/
  partials : Std.HashMap String TemplateEntry
  /-- Layout templates (name -> entry with mtime for hot reload) -/
  layouts : Std.HashMap String TemplateEntry
  /-- Last time we checked for file changes -/
  lastReloadCheck : UInt64
  deriving Inhabited

namespace Manager

/-- Create an empty manager with config -/
def empty (config : Config) : Manager :=
  { config
  , engine := Stencil.Engine.withCacheSize config.cacheSize
  , templates := {}
  , partials := {}
  , layouts := {}
  , lastReloadCheck := 0
  }

/-- Get file modification time -/
private def getFileMtime (path : System.FilePath) : IO UInt64 := do
  let metadata ← path.metadata
  pure metadata.modified.sec.toNat.toUInt64

/-- Check if a filename indicates a partial (starts with _) -/
private def isPartialFile (path : System.FilePath) : Bool :=
  match path.fileName with
  | some name => name.startsWith "_"
  | none => false

/-- Check if a path is in the layouts directory -/
private def isLayoutFile (templateDir : String) (path : System.FilePath) : Bool :=
  let pathStr := path.toString
  let layoutsDir := templateDir ++ "/layouts/"
  containsSubstr pathStr layoutsDir || containsSubstr pathStr (templateDir ++ "\\layouts\\")

/-- Compute template name from file path -/
private def computeTemplateName (templateDir : String) (extension : String) (path : System.FilePath) : String :=
  let pathStr := path.toString
  -- Remove template dir prefix
  let withoutDir := if pathStr.startsWith templateDir then
    pathStr.drop (templateDir.length + 1)  -- +1 for separator
  else
    pathStr
  -- Remove extension
  let withoutExt := if withoutDir.endsWith extension then
    withoutDir.dropRight extension.length
  else
    withoutDir
  -- Remove leading underscore for partials
  let name := if withoutExt.startsWith "_" then withoutExt.drop 1 else withoutExt
  -- Normalize path separators to /
  name.replace "\\" "/"

/-- Recursively list all files in a directory -/
partial def walkDir (dir : System.FilePath) : IO (List System.FilePath) := do
  let mut files : List System.FilePath := []
  if ← dir.isDir then
    for entry in ← dir.readDir do
      if ← entry.path.isDir then
        let subFiles ← walkDir entry.path
        files := files ++ subFiles
      else
        files := entry.path :: files
  pure files

/-- Discover and load all templates from the template directory -/
def discover (config : Config) : IO Manager := do
  let templateDir := System.FilePath.mk config.templateDir

  -- Check if template directory exists
  if !(← templateDir.pathExists) then
    IO.eprintln s!"Warning: Template directory '{config.templateDir}' does not exist"
    return empty config

  let mut manager := empty config
  let allFiles ← walkDir templateDir

  -- Filter to only template files (use endsWith to support compound extensions like .html.hbs)
  let templateFiles := allFiles.filter fun path =>
    path.toString.endsWith config.extension

  for path in templateFiles do
    let content ← IO.FS.readFile path
    match Stencil.parse content with
    | .ok tmpl =>
      let name := computeTemplateName config.templateDir config.extension path
      let mtime ← getFileMtime path

      let entry : TemplateEntry := { template := tmpl, path := path, mtime := mtime }
      if isPartialFile path then
        -- Register as partial (strip leading _ from name)
        let partialName := if name.startsWith "_" then name.drop 1 else name
        manager := { manager with partials := manager.partials.insert partialName entry }
        if config.autoRegisterPartials then
          IO.println s!"  Partial: {partialName}"
      else if isLayoutFile config.templateDir path then
        -- Register as layout (strip layouts/ prefix)
        let layoutName := if name.startsWith "layouts/" then name.drop 8 else name
        manager := { manager with layouts := manager.layouts.insert layoutName entry }
        IO.println s!"  Layout: {layoutName}"
      else
        -- Regular template
        manager := { manager with templates := manager.templates.insert name entry }
        IO.println s!"  Template: {name}"
    | .error e =>
      IO.eprintln s!"Warning: Failed to parse template {path}: {e}"

  let now ← IO.monoNanosNow
  pure { manager with lastReloadCheck := (now / 1000000).toUInt64 }  -- Convert to ms

/-- Get a template by name -/
def getTemplate (m : Manager) (name : String) : Option StencilTemplate :=
  m.templates.get? name |>.map (·.template)

/-- Get a layout by name -/
def getLayout (m : Manager) (name : String) : Option StencilTemplate :=
  m.layouts.get? name |>.map (·.template)

/-- Get a partial by name -/
def getPartial (m : Manager) (name : String) : Option StencilTemplate :=
  m.partials.get? name |>.map (·.template)

/-- Get all partials as a HashMap of templates (for Stencil context) -/
def getPartials (m : Manager) : Std.HashMap String StencilTemplate :=
  m.partials.fold (init := {}) fun acc name entry =>
    acc.insert name entry.template

/-- Check if hot reload should run (based on interval) -/
def shouldCheckReload (m : Manager) : IO Bool := do
  if !m.config.hotReload then return false
  let now ← IO.monoNanosNow
  let nowMs : UInt64 := (now / 1000000).toUInt64
  let interval : UInt64 := m.config.hotReloadIntervalMs.toUInt64
  pure (nowMs > m.lastReloadCheck + interval)

/-- Reload a single template if its mtime has changed -/
private def reloadTemplate (m : Manager) (name : String) (entry : TemplateEntry)
    : IO (Manager × Bool) := do
  let currentMtime ← getFileMtime entry.path
  if currentMtime != entry.mtime then
    IO.println s!"Hot reload: {name}"
    let content ← IO.FS.readFile entry.path
    match Stencil.parse content with
    | .ok tmpl =>
      let newEntry := { entry with template := tmpl, mtime := currentMtime }
      let newTemplates := m.templates.insert name newEntry
      pure ({ m with templates := newTemplates }, true)
    | .error e =>
      IO.eprintln s!"Hot reload error in {name}: {e}"
      pure (m, false)
  else
    pure (m, false)

/-- Reload a single layout if its mtime has changed -/
private def reloadLayout (m : Manager) (name : String) (entry : TemplateEntry)
    : IO (Manager × Bool) := do
  let currentMtime ← getFileMtime entry.path
  if currentMtime != entry.mtime then
    IO.println s!"Hot reload: layout/{name}"
    let content ← IO.FS.readFile entry.path
    match Stencil.parse content with
    | .ok tmpl =>
      let newEntry := { entry with template := tmpl, mtime := currentMtime }
      let newLayouts := m.layouts.insert name newEntry
      pure ({ m with layouts := newLayouts }, true)
    | .error e =>
      IO.eprintln s!"Hot reload error in layout/{name}: {e}"
      pure (m, false)
  else
    pure (m, false)

/-- Reload a single partial if its mtime has changed -/
private def reloadPartial (m : Manager) (name : String) (entry : TemplateEntry)
    : IO (Manager × Bool) := do
  let currentMtime ← getFileMtime entry.path
  if currentMtime != entry.mtime then
    IO.println s!"Hot reload: partial/{name}"
    let content ← IO.FS.readFile entry.path
    match Stencil.parse content with
    | .ok tmpl =>
      let newEntry := { entry with template := tmpl, mtime := currentMtime }
      let newPartials := m.partials.insert name newEntry
      pure ({ m with partials := newPartials }, true)
    | .error e =>
      IO.eprintln s!"Hot reload error in partial/{name}: {e}"
      pure (m, false)
  else
    pure (m, false)

/-- Check all templates for changes and reload if needed.
    Returns (updatedManager, templatesChanged) -/
def checkAndReload (m : Manager) : IO (Manager × Bool) := do
  if !m.config.hotReload then return (m, false)

  let mut manager := m
  let mut anyChanged := false

  -- Check regular templates
  for (name, entry) in m.templates.toList do
    let (newManager, changed) ← reloadTemplate manager name entry
    manager := newManager
    if changed then anyChanged := true

  -- Check layouts
  for (name, entry) in m.layouts.toList do
    let (newManager, changed) ← reloadLayout manager name entry
    manager := newManager
    if changed then anyChanged := true

  -- Check partials
  for (name, entry) in m.partials.toList do
    let (newManager, changed) ← reloadPartial manager name entry
    manager := newManager
    if changed then anyChanged := true

  -- Update last check time
  let now ← IO.monoNanosNow
  pure ({ manager with lastReloadCheck := (now / 1000000).toUInt64 }, anyChanged)

/-- Number of loaded templates -/
def templateCount (m : Manager) : Nat :=
  m.templates.size

/-- Number of loaded partials -/
def partialCount (m : Manager) : Nat :=
  m.partials.size

/-- Number of loaded layouts -/
def layoutCount (m : Manager) : Nat :=
  m.layouts.size

/-- Start a background watcher task that polls for template changes.
    When changes are detected, calls the provided callback.
    Returns a task that runs indefinitely. -/
def startWatcher (managerRef : IO.Ref Manager) (onChanged : IO Unit) : IO (Task (Except IO.Error Unit)) := do
  IO.asTask do
    while true do
      IO.sleep (500 : UInt32)  -- Poll every 500ms
      let manager ← managerRef.get
      if manager.config.hotReload then
        if ← manager.shouldCheckReload then
          let (updated, changed) ← manager.checkAndReload
          managerRef.set updated
          if changed then
            onChanged

end Manager

end Loom.Stencil
