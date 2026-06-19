{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RankNTypes            #-}

module DMQ.Diffusion.NodeKernel
  ( module DMQ.Diffusion.NodeKernel.Types
  , withNodeKernel
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import "contra-tracer" Control.Tracer (nullTracer)

import Data.Function (on)
import Data.Hashable
import Data.Map.Strict qualified as Map
import Data.OrdPSQ qualified as OrdPSQ
import Data.Proxy
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as Time
import Data.Void (Void, absurd)
import System.Random (StdGen)
import System.Random qualified as Random

import Network.Mux qualified as Mx

import Cardano.Network.NodeToClient qualified as Cardano.NtoC
import Cardano.Protocol.Crypto qualified as Cardano (StandardCrypto)

import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.Network.NodeToClient
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.ProtocolInfo

import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.KeepAlive (newKeepAliveRegistry)
import Ouroboros.Network.Mux qualified as Mx
import Ouroboros.Network.PeerSelection.Governor.Types
           (makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSharing (newPeerSharingAPI, newPeerSharingRegistry,
           ps_POLICY_PEER_SHARE_MAX_PEERS, ps_POLICY_PEER_SHARE_STICKY_TIME)
import Ouroboros.Network.Protocol.Handshake (Acceptable (..))
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
           noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.LocalStateQuery.Client
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import Ouroboros.Network.Snocket (Snocket, localAddressFromPath)
import Ouroboros.Network.Socket (ConnectToArgs (..),
           HandshakeCallbacks (HandshakeCallbacks), connectToNode)
import Ouroboros.Network.TxSubmission.Inbound.V2
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool (..),
           MempoolSeq (..), WithIndex (..))
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool

import DMQ.Configuration
import DMQ.Diffusion.NodeKernel.Types
import DMQ.Diffusion.PeerSelection.PeerMetric (mkPeerMetric)
import DMQ.Genesis
import DMQ.NodeToClient.LocalStateQueryClient
import DMQ.Policy qualified as Policy
import DMQ.Protocol.SigSubmission.Type (Sig (sigExpiresAt, sigId), SigId)
import DMQ.Tracer


newNodeKernel :: forall crypto ntnAddr m.
                 ( MonadLabelledSTM m
                 , MonadMVar m
                 , Ord ntnAddr
                 )
              => StdGen
              -> ShelleyGenesis
              -> m (NodeKernel crypto ntnAddr m)
newNodeKernel rng ShelleyGenesis {sgMaxKESEvolutions} = do
  publicPeerSelectionStateVar <- makePublicPeerSelectionStateVar

  keepAliveRegistry <- newKeepAliveRegistry
  peerSharingRegistry <- newPeerSharingRegistry

  mempool <- Mempool.empty
  sigChannelVar <- newTxChannelsVar
  sigMempoolSem <- newTxMempoolSem
  let (rng', rng'') = Random.splitGen rng
  sigSharedTxStateVar <- newSharedTxStateVar rng'
  (readinessVar,
    ocertCountersVar,
    stakePoolsVar,
    ledgerBigPeersVar,
    ledgerPeersVar,
    lastSigByPoolIdVar)
    <- atomically $
    (,,,,,) <$> newTVar NotReady
            <*> newTVar Map.empty
            <*> newTVar Map.empty
            <*> newTVar Nothing
            <*> newEmptyTMVar
            <*> newTVar OrdPSQ.empty

  let withPoolValidationCtx
        :: forall a. UTCTime -> Time -> (PoolValidationCtx -> (a, PoolValidationCtx)) -> STM m a
      withPoolValidationCtx utcNow now f = do
        ctx <- PoolValidationCtx utcNow now
                <$> readTVar readinessVar
                <*> readTVar stakePoolsVar
                <*> readTVar ocertCountersVar
                <*> pure sgMaxKESEvolutions
                <*> readTVar lastSigByPoolIdVar
        let (a, PoolValidationCtx {vctxOcertMap, vctxLastSigByPoolId}) = f ctx
        writeTVar ocertCountersVar vctxOcertMap
        writeTVar lastSigByPoolIdVar vctxLastSigByPoolId
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

  peerMetric <- mkPeerMetric

  pure NodeKernel { keepAliveRegistry
                  , peerSharingRegistry
                  , peerSharingAPI
                  , mempool
                  , sigChannelVar
                  , sigMempoolSem
                  , sigSharedTxStateVar
                  , readinessVar
                  , stakePools
                  , peerMetric
                  , lastSigByPoolIdVar
                  }


withNodeKernel :: forall crypto ntnAddr ntcAddr m a.
                  ( Alternative   (STM m)
                  , MonadAsync         m
                  , MonadEvaluate      m
                  , MonadFork          m
                  , MonadDelay         m
                  , MonadLabelledSTM   m
                  , MonadMask          m
                  , MonadMVar          m
                  , Mx.MonadReadBuffer m
                  , MonadST            m
                  , MonadThrow    (STM m)
                  , MonadTime          m
                  , MonadTimer         m
                  , Ord ntnAddr
                  , Hashable ntnAddr
                  )
               => DMQTracers crypto ntnAddr ntcAddr m
               -> Snocket m Cardano.NtoC.LocalSocket LocalAddress
               -> Mx.MakeBearer m Cardano.NtoC.LocalSocket
               -> Configuration
               -> ShelleyGenesis
               -> StdGen
               -> (NodeKernel crypto ntnAddr m -> m a)
               -- ^ as soon as the callback exits the `mempoolWorker` and all
               -- decision logic threads will be killed
               -> m a
withNodeKernel DMQTracers { sigSubmissionLogicTracer,
                            localStateQueryClientTracer
                          }
               localSnocket
               mkLocalBearer
               Configuration {
                 dmqcCardanoNetworkMagic = I networkMagic,
                 dmqcCardanoNodeSocket   = I cardanoNodeSocketPath,
                 dmqcLedgerPeers         = I ledgerPeers
               }
               shelleyGenesis
               rng
               k = do
  nodeKernel@NodeKernel { mempool,
                          sigChannelVar,
                          sigSharedTxStateVar
                        }
    <- newNodeKernel rng shelleyGenesis
  withAsync (mempoolWorker mempool)
          $ \mempoolThread ->
    withAsync (decisionLogicThreads
                sigSubmissionLogicTracer
                nullTracer
                Policy.sigDecisionPolicy
                sigChannelVar
                sigSharedTxStateVar)
            $ \sigLogicThread ->
      withAsync (connectToCardanoNode nodeKernel) \spmAid -> do
        link mempoolThread
        link sigLogicThread
        link spmAid
        k nodeKernel
  where
    connectToCardanoNode :: NodeKernel crypto ntnAddr m
                         -> m (Either SomeException Void)
    connectToCardanoNode nodeKernel =
      fmap fn <$>
      connectToNode
        localSnocket
        mkLocalBearer
        ConnectToArgs {
          ctaHandshakeCodec      = Cardano.NtoC.nodeToClientHandshakeCodec,
          ctaHandshakeTimeLimits = noTimeLimitsHandshake,
          ctaVersionDataCodec    = cborTermVersionDataCodec Cardano.NtoC.nodeToClientCodecCBORTerm,
          ctaConnectTracers      = Cardano.NtoC.nullNetworkConnectTracers, --debuggingNetworkConnectTracers,
          ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
        }
        (\_ -> return ())
        (Cardano.NtoC.combineVersions
          [ Cardano.NtoC.simpleSingletonVersions
              version
              Cardano.NtoC.NodeToClientVersionData {
                  Cardano.NtoC.networkMagic
                , Cardano.NtoC.query = False
              }
              \_version ->
                Mx.OuroborosApplication
                  [ Mx.MiniProtocol
                      { Mx.miniProtocolNum = Mx.MiniProtocolNum 7
                      , Mx.miniProtocolStart = Mx.StartEagerly
                      , Mx.miniProtocolLimits =
                          Mx.MiniProtocolLimits
                            { Mx.maximumIngressQueue = 0xffffffff
                            }
                      , Mx.miniProtocolRun =
                          Mx.InitiatorProtocolOnly
                            . Mx.mkMiniProtocolCbFromPeerSt
                            . const
                            $ ( nullTracer -- TODO: add tracer
                              , cStateQueryCodec
                              , StateIdle
                              , localStateQueryClientPeer $
                                cardanoLocalStateQueryClient
                                  localStateQueryClientTracer
                                  ledgerPeers
                                  (stakePools nodeKernel)
                                  (readinessVar nodeKernel)
                              )
                      }
                  ]
          | version <- [minBound..maxBound]
          , let -- NOTE: the query protocol is running using
                -- `Cardano.StandardCrypto`, while `dmq-node` is using
                -- `StandardCrypto` defined in `kes-agent-krypto`.  These two
                -- must agree, but even if they did not, all the queries we do
                -- are crypto agnostic.
                supportedVersionMap =
                  supportedNodeToClientVersions (Proxy :: Proxy (CardanoBlock Cardano.StandardCrypto))
                blk = supportedVersionMap Map.! version
                Codecs {cStateQueryCodec} =
                  clientCodecs (pClientInfoCodecConfig . protocolClientInfoCardano $ Policy.cardanoEpochSlots)
                  blk version
          ])
        Nothing
        (localAddressFromPath cardanoNodeSocketPath)
      where
        fn :: forall x. Either x Void -> x
        fn = either id absurd


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
