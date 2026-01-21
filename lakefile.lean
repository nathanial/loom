import Lake
open Lake DSL

package loom where
  version := v!"0.1.3"

require citadel from git "https://github.com/nathanial/citadel" @ "v0.0.1"
require scribe from git "https://github.com/nathanial/scribe" @ "v0.0.2"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.8"
require ledger from git "https://github.com/nathanial/ledger" @ "v0.0.2"
require chronicle from git "https://github.com/nathanial/chronicle" @ "v0.0.1"
require staple from git "https://github.com/nathanial/staple" @ "v0.0.2"
require stencil from git "https://github.com/nathanial/stencil" @ "v0.1.6"

-- OpenSSL linking (required by citadel's TLS support)
-- Lake doesn't propagate moreLinkArgs from dependencies, so we must add them here
def opensslLinkArgs : Array String :=
  #["-L/opt/homebrew/opt/openssl@3/lib", "-lssl", "-lcrypto"]

@[default_target]
lean_lib Loom where
  roots := #[`Loom]
  moreLinkArgs := opensslLinkArgs

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe loom_tests where
  root := `Tests.Main
  moreLinkArgs := opensslLinkArgs
