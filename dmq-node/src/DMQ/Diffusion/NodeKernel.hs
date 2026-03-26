{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes     #-}

module DMQ.Diffusion.NodeKernel
  ( module DMQ.Diffusion.NodeKernel.Types
  , withNodeKernel
  ) where

import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import "contra-tracer" Control.Tracer (Tracer, nullTracer)

import Data.Function (on)
import Data.Functor.Contravariant ((>$<))
import Data.Hashable
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as Time
import Data.Void (Void)
import System.Random (StdGen)
import System.Random qualified as Random

import Ouroboros.Network.BlockFetch (newFetchClientRegistry)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSharing (newPeerSharingAPI, newPeerSharingRegistry,
           ps_POLICY_PEER_SHARE_MAX_PEERS, ps_POLICY_PEER_SHARE_STICKY_TIME)
import Ouroboros.Network.TxSubmission.Inbound.V2
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool (..),
           MempoolSeq (..), WithIndex (..))
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool

import DMQ.Configuration
import DMQ.Diffusion.NodeKernel.Types
import DMQ.Policy qualified as Policy
import DMQ.Protocol.SigSubmission.Type (Sig (sigExpiresAt, sigId), SigId)
import DMQ.Tracer

newNodeKernel :: forall crypto ntnAddr m.
                 ( MonadLabelledSTM m
                 , MonadMVar m
                 , Ord ntnAddr
                 )
              => StdGen
              -> m (NodeKernel crypto ntnAddr m)
newNodeKernel rng = do
  publicPeerSelectionStateVar <- makePublicPeerSelectionStateVar

  fetchClientRegistry <- newFetchClientRegistry
  peerSharingRegistry <- newPeerSharingRegistry

  mempool <- Mempool.empty
  sigChannelVar <- newTxChannelsVar
  sigMempoolSem <- newTxMempoolSem
  let (rng', rng'') = Random.splitGen rng
  sigSharedTxStateVar <- newSharedTxStateVar rng'
  (nextEpochVar, ocertCountersVar, stakePoolsVar, ledgerBigPeersVar, ledgerPeersVar) <- atomically $
    (,,,,) <$> newTVar Nothing
           <*> newTVar Map.empty
           <*> newTVar Map.empty
           <*> newTVar Nothing
           <*> newEmptyTMVar

  let withPoolValidationCtx
        :: forall a. (PoolValidationCtx -> (a, PoolValidationCtx)) -> STM m a
      withPoolValidationCtx f = do
        ctx <- PoolValidationCtx <$> readTVar nextEpochVar
                                 <*> readTVar stakePoolsVar
                                 <*> readTVar ocertCountersVar
        let (a, PoolValidationCtx {vctxOcertMap}) = f ctx
        writeTVar ocertCountersVar vctxOcertMap
        return a

      stakePools = StakePools {
          stakePoolsVar,
          withPoolValidationCtx,
          ledgerBigPeersVar,
          ledgerPeersVar
        }

  peerSharingAPI <-
    newPeerSharingAPI
      publicPeerSelectionStateVar
      rng''
      ps_POLICY_PEER_SHARE_STICKY_TIME
      ps_POLICY_PEER_SHARE_MAX_PEERS

  pure NodeKernel { fetchClientRegistry
                  , peerSharingRegistry
                  , peerSharingAPI
                  , mempool
                  , sigChannelVar
                  , sigMempoolSem
                  , sigSharedTxStateVar
                  , nextEpochVar
                  , stakePools
                  }


withNodeKernel :: forall crypto ntnAddr m a.
                  ( MonadAsync       m
                  , MonadFork        m
                  , MonadDelay       m
                  , MonadLabelledSTM m
                  , MonadMask        m
                  , MonadMVar        m
                  , MonadTime        m
                  , Ord ntnAddr
                  , Show ntnAddr
                  , Hashable ntnAddr
                  )
               => Tracer m WithEventType
               -> Configuration
               -> StdGen
               -> (NetworkMagic -> NodeKernel crypto ntnAddr m -> m (Either SomeException Void))
               -> (NodeKernel crypto ntnAddr m -> m a)
               -- ^ as soon as the callback exits the `mempoolWorker` and all
               -- decision logic threads will be killed
               -> m a
withNodeKernel tracer
               Configuration {
                 dmqcCardanoNetworkMagic = I networkMagic
               }
               rng
               mkStakePoolMonitor k = do
  nodeKernel@NodeKernel { mempool,
                          sigChannelVar,
                          sigSharedTxStateVar
                        }
    <- newNodeKernel rng
  withAsync (mempoolWorker mempool)
          $ \mempoolThread ->
    withAsync (decisionLogicThreads
                (WithEventType (DMQ "SigSubmission.Logic") >$< tracer)
                nullTracer
                Policy.sigDecisionPolicy
                sigChannelVar
                sigSharedTxStateVar)
            $ \sigLogicThread ->
      withAsync (mkStakePoolMonitor networkMagic nodeKernel) \spmAid -> do
        link mempoolThread
        link sigLogicThread
        link spmAid
        k nodeKernel


mempoolWorker :: forall crypto m.
                 ( MonadDelay m
                 , MonadSTM   m
                 , MonadTime  m
                 )
              => Mempool m SigId (Sig crypto)
              -> m Void
mempoolWorker (Mempool v) = loop
  where
    loop = do
      now <- getCurrentPOSIXTime
      rt <- atomically $ do
        mempool@MempoolSeq { mempoolSeq, mempoolSet } <- readTVar v
        let mempoolSeq' :: Seq (WithIndex (Sig crypto))
            mempoolSet', expiredSet' :: Set SigId

            (resumeTime, expiredSet', mempoolSeq') =
              foldr (\a@WithIndex { getTx = sig } (rt, expiredSet, sigs) ->
                      if sigExpiresAt sig <= now
                      then ( rt
                           , sigId sig `Set.insert` expiredSet
                           , sigs
                           )
                      else ( rt `min` sigExpiresAt sig
                           , expiredSet
                           , a Seq.<| sigs
                           )
                    )
                    (now, Set.empty, Seq.empty)
                    mempoolSeq

            mempoolSet' = mempoolSet `Set.difference` expiredSet'

        writeTVar v mempool { mempoolSet = mempoolSet',
                              mempoolSeq = mempoolSeq' }
        return resumeTime

      now' <- getCurrentPOSIXTime
      threadDelay $ rt `diffPOSIXTime` now' `max` _MEMPOOL_WORKER_MIN_DELAY

      loop



_MEMPOOL_WORKER_MIN_DELAY :: DiffTime
_MEMPOOL_WORKER_MIN_DELAY = 0.05


--
-- POSIXTime utils
--


getCurrentPOSIXTime :: MonadTime m
                    => m POSIXTime
getCurrentPOSIXTime = Time.utcTimeToPOSIXSeconds <$> getCurrentTime


diffPOSIXTime :: POSIXTime -> POSIXTime -> DiffTime
diffPOSIXTime = on diffTime (Time . posixTimeToDiffTime)
  where
    posixTimeToDiffTime :: POSIXTime -> DiffTime
    posixTimeToDiffTime = realToFrac
