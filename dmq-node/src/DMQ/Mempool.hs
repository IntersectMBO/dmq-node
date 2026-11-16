module DMQ.Mempool
  ( module Mempool
  , empty
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Ouroboros.Network.TxSubmission.Mempool.Simple as Mempool hiding (empty)

-- | An empty mempool.
--
-- Not ouroboros-network's `Mempool.empty`: it gives the first entry index -1,
-- which is also `mempoolZeroIdx`, so readers asking for entries after it never
-- see that entry.
empty :: (MonadSTM m, Ord txid) => (tx -> txid) -> m (Mempool m txid tx)
empty getTxId = new getTxId []
