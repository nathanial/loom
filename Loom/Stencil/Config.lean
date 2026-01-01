/-
  Loom.Stencil.Config - Configuration for Stencil template integration
-/

namespace Loom.Stencil

/-- Configuration for the Stencil template engine integration -/
structure Config where
  /-- Base directory for template files -/
  templateDir : String := "templates"
  /-- File extension for template files (including the dot) -/
  extension : String := ".stencil"
  /-- Enable hot reloading in development (checks file mtimes) -/
  hotReload : Bool := true
  /-- How often to check for file changes in milliseconds -/
  hotReloadIntervalMs : Nat := 500
  /-- Maximum number of templates to cache -/
  cacheSize : Nat := 1000
  /-- Automatically register files starting with _ as partials -/
  autoRegisterPartials : Bool := true
  /-- Default layout template to wrap all renders (None = no layout) -/
  defaultLayout : Option String := none
  deriving Repr, Inhabited

namespace Config

/-- Default configuration -/
def default : Config := {}

/-- Configuration for development (hot reload enabled) -/
def development : Config := { hotReload := true }

/-- Configuration for production (hot reload disabled) -/
def production : Config := { hotReload := false }

/-- Set the template directory -/
def withTemplateDir (c : Config) (dir : String) : Config :=
  { c with templateDir := dir }

/-- Set the file extension -/
def withExtension (c : Config) (ext : String) : Config :=
  { c with extension := ext }

/-- Enable or disable hot reload -/
def withHotReload (c : Config) (enabled : Bool) : Config :=
  { c with hotReload := enabled }

/-- Set the default layout -/
def withDefaultLayout (c : Config) (layout : String) : Config :=
  { c with defaultLayout := some layout }

end Config

end Loom.Stencil
