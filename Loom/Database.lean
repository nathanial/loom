/-
  Loom.Database - Database integration with Ledger
-/
import Ledger

namespace Loom

namespace Database

/-- Factory function that creates a new database connection per-request. -/
abbrev ConnectionFactory := IO Ledger.Connection

/-- Default factory creates an empty in-memory connection. -/
def defaultFactory : ConnectionFactory := pure Ledger.Connection.create

/-- Database configuration for the application. -/
structure DbConfig where
  /-- Factory function to create connections. -/
  factory : ConnectionFactory
  /-- Whether to log queries to stderr. -/
  logQueries : Bool := false
  deriving Inhabited

namespace DbConfig

/-- Create a config with default settings. -/
def default : DbConfig := { factory := defaultFactory }

/-- Create a config with a custom factory. -/
def withFactory (factory : ConnectionFactory) : DbConfig :=
  { factory := factory }

end DbConfig

end Database

end Loom
