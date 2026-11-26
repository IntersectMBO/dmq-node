module DMQ.Mempool
  ( module Mempool
  , empty
  , getWriter
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Ouroboros.Network.TxSubmission.Mempool.Simple as Mempool hiding (empty,
           getWriter)

-- | An empty mempool.
--
-- Not ouroboros-network's `Mempool.empty`: it gives the first entry index -1,
-- which is also `mempoolZeroIdx`, so readers asking for entries after it never
-- see that entry.
empty :: (MonadSTM m, Ord txid) => (tx -> txid) -> m (Mempool m txid tx)
empty getTxId = new getTxId []

getWriter
  :: forall tx txid failure m.
     ( MonadSTM m
     , MonadTime m
     , MonadMonotonicTime m
     , Ord txid
     )
  => failure
  -- ^ duplicate tx error
  -> (tx -> txid)
  -- ^ get transaction hash
  -> ((UTCTime, Time) -> [tx] -> STM m [Either (txid, failure) tx])
  -- ^ validate a tx in an `STM` transaction, this allows acquiring and
  -- updating validation context.
  -> ([(txid, failure)] -> m ())
  -- ^ handle invalid txs, e.g. logging, throwing exceptions, etc
  -> Mempool m txid tx
  -- ^ mempool
  -> TxSubmissionMempoolWriter txid tx Integer m failure
getWriter = getWriterWithCtx (\_ -> (,) <$> getCurrentTime <*> getMonotonicTime)

