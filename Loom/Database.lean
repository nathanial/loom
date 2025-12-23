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

/-- Persistent connection factory creates a connection backed by JSONL file. -/
def persistentFactory (journalPath : System.FilePath) : IO Ledger.Persist.PersistentConnection :=
  Ledger.Persist.PersistentConnection.create journalPath

/-- Database configuration for the application. -/
structure DbConfig where
  /-- Factory function to create in-memory connections. -/
  factory : ConnectionFactory
  /-- Path to JSONL journal file for persistence (None for in-memory only). -/
  journalPath : Option System.FilePath := none
  /-- Whether to log queries to stderr. -/
  logQueries : Bool := false
  deriving Inhabited

namespace DbConfig

/-- Create a config with default settings (in-memory). -/
def default : DbConfig := { factory := defaultFactory }

/-- Create a config with a custom factory. -/
def withFactory (factory : ConnectionFactory) : DbConfig :=
  { factory := factory }

/-- Create a config with persistent storage. -/
def withPersistence (path : System.FilePath) : DbConfig :=
  { factory := defaultFactory, journalPath := some path }

/-- Check if this config uses persistent storage. -/
def isPersistent (config : DbConfig) : Bool :=
  config.journalPath.isSome

end DbConfig

end Database

end Loom
