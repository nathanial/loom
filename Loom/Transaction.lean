/-
  Loom.Transaction - Transaction monad integration

  Re-exports Ledger.TxM and provides ActionM integration via runTx/withNewEntity.
  See Loom.ActionM for the ActionM integration functions.
-/
import Ledger

namespace Loom

-- Re-export core types for convenience
export Ledger (TxState TxM)

end Loom
