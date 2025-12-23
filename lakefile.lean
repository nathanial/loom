import Lake
open Lake DSL

package loom where
  version := v!"0.1.0"

require citadel from ".." / "citadel"
require scribe from ".." / "scribe"
require crucible from ".." / "crucible"
require ledger from ".." / "ledger"

@[default_target]
lean_lib Loom where
  roots := #[`Loom]

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe loom_tests where
  root := `Tests.Main
