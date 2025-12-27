/-
  Loom.Audit - Monadic audit logging for database transactions

  AuditTxM is a monad that wraps TxM to accumulate audit entries alongside
  database operations. After the transaction commits, all audit entries
  are automatically logged.

  ## Usage

  ```lean
  let (eid, _) ← withNewEntityAudit! fun eid => do
    DbColumn.TxM.create eid dbCol
    audit "CREATE" "column" eid.id.toNat [("name", name)]
  -- Audit entries are logged automatically after commit
  ```
-/
import Ledger
import Chronicle
import Loom.Controller

namespace Loom

/-! ## Audit Entry -/

/-- A single audit log entry -/
structure AuditEntry where
  /-- Operation type (CREATE, UPDATE, DELETE, etc.) -/
  op : String
  /-- Entity type (column, card, user, etc.) -/
  entity : String
  /-- Entity ID -/
  entityId : Nat
  /-- Additional key-value details -/
  details : List (String × String) := []
  deriving Repr, Inhabited

/-! ## Audit Transaction State -/

/-- Combined state for transaction ops and audit entries -/
structure AuditTxState where
  /-- Accumulated transaction operations -/
  txState : Ledger.TxState := {}
  /-- Accumulated audit entries -/
  auditEntries : Array AuditEntry := #[]
  deriving Repr, Inhabited

/-! ## Audit Transaction Monad -/

/-- Transaction + Audit monad: builds tx ops and audit entries with db read access -/
abbrev AuditTxM := StateT AuditTxState (ReaderT Ledger.Db Id)

namespace AuditTxM

/-! ## Core Operations -/

/-- Record an audit entry to be logged after transaction commits -/
def audit (op entity : String) (entityId : Nat)
    (details : List (String × String) := []) : AuditTxM Unit :=
  modify fun s =>
    let entry : AuditEntry := { op := op, entity := entity, entityId := entityId, details := details }
    { s with auditEntries := s.auditEntries.push entry }

/-- Get the database snapshot (for reading current values) -/
def getDb : AuditTxM Ledger.Db := read

/-- Lift a TxM operation into AuditTxM -/
def liftTx (m : Ledger.TxM α) : AuditTxM α := do
  let db ← read
  let s ← get
  -- TxM α = StateT TxState (ReaderT Db Id) α
  -- StateT.run : StateT σ m α → σ → m (α × σ)
  -- ReaderT.run : ReaderT ρ m α → ρ → m α
  let readerResult : ReaderT Ledger.Db Id (α × Ledger.TxState) := StateT.run m s.txState
  let (result, newTxState) : α × Ledger.TxState := ReaderT.run readerResult db
  set { s with txState := newTxState }
  pure result

/-- Automatic coercion from TxM to AuditTxM -/
instance : Coe (Ledger.TxM α) (AuditTxM α) where
  coe := liftTx

/-! ## Transaction Operations (delegated to TxM) -/

/-- Add an assertion (add fact) -/
def add (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Ledger.Value) : AuditTxM Unit :=
  liftTx (Ledger.TxM.add e attr v)

/-- Add a retraction (remove fact) -/
def retract (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Ledger.Value) : AuditTxM Unit :=
  liftTx (Ledger.TxM.retract e attr v)

/-- Add a string value -/
def addStr (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : String) : AuditTxM Unit :=
  liftTx (Ledger.TxM.addStr e attr v)

/-- Add an int value -/
def addInt (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Int) : AuditTxM Unit :=
  liftTx (Ledger.TxM.addInt e attr v)

/-- Add a nat value (as int) -/
def addNat (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Nat) : AuditTxM Unit :=
  liftTx (Ledger.TxM.addNat e attr v)

/-- Add a bool value -/
def addBool (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Bool) : AuditTxM Unit :=
  liftTx (Ledger.TxM.addBool e attr v)

/-- Add a reference (EntityId) -/
def addRef (e : Ledger.EntityId) (attr : Ledger.Attribute) (ref : Ledger.EntityId) : AuditTxM Unit :=
  liftTx (Ledger.TxM.addRef e attr ref)

/-- Add a float value -/
def addFloat (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Float) : AuditTxM Unit :=
  liftTx (Ledger.TxM.addFloat e attr v)

/-- Retract a string value -/
def retractStr (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : String) : AuditTxM Unit :=
  liftTx (Ledger.TxM.retractStr e attr v)

/-- Retract an int value -/
def retractInt (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Int) : AuditTxM Unit :=
  liftTx (Ledger.TxM.retractInt e attr v)

/-- Retract a bool value -/
def retractBool (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Bool) : AuditTxM Unit :=
  liftTx (Ledger.TxM.retractBool e attr v)

/-- Retract a reference -/
def retractRef (e : Ledger.EntityId) (attr : Ledger.Attribute) (ref : Ledger.EntityId) : AuditTxM Unit :=
  liftTx (Ledger.TxM.retractRef e attr ref)

/-- Set a string value with cardinality-one enforcement -/
def setStr (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : String) : AuditTxM Unit :=
  liftTx (Ledger.TxM.setStr e attr v)

/-- Set an int value with cardinality-one enforcement -/
def setInt (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Int) : AuditTxM Unit :=
  liftTx (Ledger.TxM.setInt e attr v)

/-- Set a nat value with cardinality-one enforcement -/
def setNat (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Nat) : AuditTxM Unit :=
  liftTx (Ledger.TxM.setNat e attr v)

/-- Set a bool value with cardinality-one enforcement -/
def setBool (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Bool) : AuditTxM Unit :=
  liftTx (Ledger.TxM.setBool e attr v)

/-- Set a reference with cardinality-one enforcement -/
def setRef (e : Ledger.EntityId) (attr : Ledger.Attribute) (ref : Ledger.EntityId) : AuditTxM Unit :=
  liftTx (Ledger.TxM.setRef e attr ref)

/-- Set a float value with cardinality-one enforcement -/
def setFloat (e : Ledger.EntityId) (attr : Ledger.Attribute) (v : Float) : AuditTxM Unit :=
  liftTx (Ledger.TxM.setFloat e attr v)

/-- Retract all values for an attribute (for deletion) -/
def retractAttr (e : Ledger.EntityId) (attr : Ledger.Attribute) : AuditTxM Unit :=
  liftTx (Ledger.TxM.retractAttr e attr)

/-- Retract all values for a list of attributes -/
def retractAttrs (e : Ledger.EntityId) (attrs : List Ledger.Attribute) : AuditTxM Unit :=
  liftTx (Ledger.TxM.retractAttrs e attrs)

/-! ## Building -/

/-- Build the transaction ops and audit entries from an AuditTxM computation -/
def build (m : AuditTxM α) (db : Ledger.Db) : α × Array Ledger.TxOp × Array AuditEntry :=
  let (result, state) := m.run {} |>.run db
  (result, state.txState.ops, state.auditEntries)

end AuditTxM

/-! ## Audit Logging Helper -/

/-- Get current user ID from session (or "anonymous") -/
private def getUserId (ctx : Context) : String :=
  ctx.session.get "user_id" |>.getD "anonymous"

/-- Log an audit entry to the Chronicle logger -/
def logAudit (ctx : Context) (op : String) (entity : String) (entityId : Nat)
    (details : List (String × String) := []) : IO Unit := do
  match ctx.logger with
  | none => pure ()
  | some logger =>
    let userId := getUserId ctx
    let entry : Chronicle.LogEntry := {
      timestamp := ← IO.monoNanosNow
      level := .info
      message := s!"[AUDIT] {op} {entity}"
      context := [("user_id", userId), ("entity_id", toString entityId)] ++ details
    }
    Chronicle.MultiLogger.logRequest logger entry

/-- Log a warning audit entry -/
def logAuditWarn (ctx : Context) (op : String) (entity : String) (entityId : Nat)
    (details : List (String × String) := []) : IO Unit := do
  match ctx.logger with
  | none => pure ()
  | some logger =>
    let userId := getUserId ctx
    let entry : Chronicle.LogEntry := {
      timestamp := ← IO.monoNanosNow
      level := .warn
      message := s!"[AUDIT] {op} {entity}"
      context := [("user_id", userId), ("entity_id", toString entityId)] ++ details
    }
    Chronicle.MultiLogger.logRequest logger entry

/-- Log an error audit entry -/
def logAuditError (ctx : Context) (op : String) (entity : String)
    (details : List (String × String) := []) : IO Unit := do
  match ctx.logger with
  | none => pure ()
  | some logger =>
    let userId := getUserId ctx
    let entry : Chronicle.LogEntry := {
      timestamp := ← IO.monoNanosNow
      level := .error
      message := s!"[AUDIT] {op} {entity} FAILED"
      context := [("user_id", userId)] ++ details
    }
    Chronicle.MultiLogger.logRequest logger entry

end Loom
