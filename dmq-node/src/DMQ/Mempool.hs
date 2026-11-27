module DMQ.Mempool
  ( module Mempool
  , empty
  , getIndexedReader
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map

import Ouroboros.Network.SizeInBytes (SizeInBytes)
import Ouroboros.Network.TxSubmission.Mempool.Reader (MempoolSnapshot (..))
import Ouroboros.Network.TxSubmission.Mempool.Simple as Mempool hiding (empty)

-- | An empty mempool.
--
-- Not ouroboros-network's `Mempool.empty`: it gives the first entry index -1,
-- which is also `mempoolZeroIdx`, so readers asking for entries after it never
-- see that entry.
empty :: (MonadSTM m, Ord txid) => (tx -> txid) -> m (Mempool m txid tx)
empty getTxId = new getTxId []


-- | Legacy outbound peers look up the issuing pool of each candidate. Share
-- one index across those lookups in a snapshot. ID-only queries stay lazy.
getIndexedReader :: (MonadSTM m, Ord sigId)
                 => (sig -> sigId)
                 -> (sig -> SizeInBytes)
                 -> Mempool m sigId sig
                 -> TxSubmissionMempoolReader sigId sig Integer m
getIndexedReader idOf sizeOf mempool@(Mempool contents) =
    reader { mempoolGetSnapshot = do
               snapshot <- mempoolGetSnapshot reader
               MempoolSeq {mempoolSeq} <- readTVar contents
               let byIndex = Map.fromDistinctAscList
                       [ (idx, sig) | WithIndex idx sig <- toList mempoolSeq ]
               pure snapshot { mempoolLookupTx = (`Map.lookup` byIndex) }
           }
  where
    reader = getReader idOf sizeOf mempool
