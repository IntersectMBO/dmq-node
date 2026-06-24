module DMQ.Mempool
  ( module Mempool
  , getWriter
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Ouroboros.Network.TxSubmission.Mempool.Simple as Mempool hiding
           (getWriter)

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

