{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}

module DMQ.NodeToClient.LocalStateQueryClient
  ( TraceLocalStateQueryClient (..)
  , cardanoClient
  , connectToCardanoNode
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (force)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Trans.Except
import Control.Tracer (Tracer (..), nullTracer, traceWith)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.Void

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Ledger.Api.State.Query (StakeSnapshots (..))
import Cardano.Network.NodeToClient
import Cardano.Network.PeerSelection (LedgerPeerSnapshot(..), LedgerRelayAccessPoint (..))
import Cardano.Slotting.EpochInfo.API
import Cardano.Slotting.Slot (EpochNo)
import Cardano.Slotting.Time
import DMQ.Diffusion.NodeKernel
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import Ouroboros.Consensus.HardFork.History.EpochInfo (interpreterToEpochInfo)
import Ouroboros.Consensus.HardFork.History.Qry (PastHorizonException)
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Network.NodeToClient
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Shelley.Ledger.Query
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
import Ouroboros.Network.Magic
import Ouroboros.Network.Mux qualified as Mx
import Ouroboros.Network.PeerSelection.LedgerPeers (SomeLedgerPeerSnapshot(..), LedgerPeersKind (..), accumulateBigLedgerStake)
import Ouroboros.Network.Point (Block(..))
import Ouroboros.Network.Protocol.LocalStateQuery.Client
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (SomeHashableBlock)

-- | Trace type
--
data TraceLocalStateQueryClient =
    Acquiring (Maybe SystemStart)
  | CurrentEpoch EpochNo
  | NextEpoch UTCTime NominalDiffTime
  | PastHorizon PastHorizonException

instance ToJSON TraceLocalStateQueryClient where
  toJSON = \case
    Acquiring mSystemStart ->
      object [ "kind" .= Aeson.String "Acquiring"
             , "systemStart" .= show mSystemStart
             ]
    CurrentEpoch epoch ->
      object [ "kind" .= Aeson.String "CurrentEpoch"
             , "remoteEpoch" .= show epoch
             ]
    NextEpoch ne timer ->
      object [ "kind" .= Aeson.String "NextEpoch"
             , "remoteTime" .= show ne
             , "countdown" .= show timer ]
    PastHorizon e ->
      object [ "kind" .= Aeson.String "PastHorizon"
             , "error" .= show e
             ]

-- TODO generalize to handle ledger eras other than Conway
-- | connects the dmq node to cardano node via local state query
-- and updates the node kernel with stake pool data necessary to perform message
-- validation
--
cardanoClient
  :: forall block query point crypto m. (MonadDelay m, MonadSTM m, MonadThrow m, MonadTime m)
  => (block ~ CardanoBlock crypto, query ~ Query block, point ~ Point block)
  => Tracer m TraceLocalStateQueryClient
  -> Bool -- ^ use ledger peers
  -> StakePools m
  -> StrictTVar m (Maybe UTCTime) -- ^ from node kernel
  -> LocalStateQueryClient (CardanoBlock crypto) (Point block) (Query block) m Void
cardanoClient tracer ledgerPeers
              StakePools {
                stakePoolsVar,
                ledgerPeersVar,
                ledgerBigPeersVar
              }
              nextEpochVar =
  LocalStateQueryClient (idle Nothing)
  where
    idle mSystemStart = do
      traceWith tracer $ Acquiring mSystemStart
      -- FIXME: switched to volatiletip for prerelease testing purposes
      pure $ SendMsgAcquire VolatileTip {-ImmutableTip-} acquire
      where
        acquire :: ClientStAcquiring block point query m Void
        acquire = ClientStAcquiring {
          recvMsgAcquired =
            let epochQry systemStart = pure $
                    SendMsgQuery (BlockQuery . QueryIfCurrentConway $ GetEpochNo)
                  $ wrappingMismatch (handleEpoch systemStart)
            in case mSystemStart of
                 Just systemStart -> epochQry systemStart
                 Nothing -> pure $
                   SendMsgQuery GetSystemStart $ ClientStQuerying epochQry

         , recvMsgFailure = \failure ->
             throwIO . userError $ "recvMsgFailure: " <> show failure
         }

    wrappingMismatch k = ClientStQuerying $
      either (const . throwIO . userError $ "mismatch era info") k

    handleEpoch systemStart epoch = pure
      . SendMsgQuery (BlockQuery . QueryHardFork $ GetInterpreter)
      $ getInterpreter systemStart epoch

    getInterpreter systemStart epoch = ClientStQuerying \interpreter -> do
      let ei  = interpreterToEpochInfo interpreter
          res =
            runExcept do
              lastSlot <- snd <$> epochInfoRange ei epoch
              lastSlotTime <- epochInfoSlotToRelativeTime ei lastSlot
              lastSlotLength <- epochInfoSlotLength ei lastSlot
              pure $ addRelativeTime (getSlotLength lastSlotLength) lastSlotTime

      traceWith tracer $ CurrentEpoch epoch
      case res of
        Left err ->
          pure $ SendMsgRelease do
            traceWith tracer $ PastHorizon err
            threadDelay 86400
            idle $ Just systemStart
        Right relativeTime -> do
          let nextEpoch = fromRelativeTime systemStart relativeTime
          -- continue with stake snapshot query
          pure $ queryStakeSnapshots systemStart nextEpoch


    -- query stake snapshot
    queryStakeSnapshots
      :: SystemStart
      -> UTCTime
      -> ClientStAcquired
           (CardanoBlock crypto)
           (Point (CardanoBlock crypto))
           (Query (CardanoBlock crypto))
           m
           Void
    queryStakeSnapshots systemStart nextEpoch =
        SendMsgQuery (BlockQuery . QueryIfCurrentConway $ GetStakeSnapshots Nothing)
          $ wrappingMismatch handleStakeSnapshots
      where
        handleStakeSnapshots
          :: StakeSnapshots
          -> m (ClientStAcquired
                  (CardanoBlock crypto)
                  (Point (CardanoBlock crypto))
                  (Query (CardanoBlock crypto))
                  m
                  Void)
        handleStakeSnapshots StakeSnapshots { ssStakeSnapshots } = do
          atomically do
            writeTVar stakePoolsVar ssStakeSnapshots
            writeTVar nextEpochVar $ Just nextEpoch
          toNextEpoch <- diffUTCTime nextEpoch <$> getCurrentTime
          traceWith tracer $ NextEpoch nextEpoch toNextEpoch
          threadDelay $ min (max 1 $ realToFrac toNextEpoch) 86400 -- TODO fuzz this?
          pure $
            if ledgerPeers
            then -- continue with ledger peers query
                 queryLedgerPeers systemStart toNextEpoch
            else -- release and continue in the idle state
            release systemStart toNextEpoch


    -- query ledger peer snapshot
    queryLedgerPeers
      :: SystemStart
      -> NominalDiffTime
      -> ClientStAcquired
           (CardanoBlock crypto)
           (Point (CardanoBlock crypto))
           (Query (CardanoBlock crypto))
           m
           Void
    queryLedgerPeers systemStart toNextEpoch =
        SendMsgQuery (BlockQuery . QueryIfCurrentConway $ GetLedgerPeerSnapshot AllLedgerPeers)
        $ wrappingMismatch handleLedgerPeers
      where
        handleLedgerPeers
          :: SomeLedgerPeerSnapshot
          -> m (ClientStAcquired
                  (CardanoBlock crypto)
                  (Point (CardanoBlock crypto))
                  (Query (CardanoBlock crypto))
                  m
                  Void)
        handleLedgerPeers (SomeLedgerPeerSnapshot _ (LedgerAllPeerSnapshotV23 pt magic peers)) = do
          let bigSrvRelays = force
                [(accStake, (stake, NonEmpty.fromList relays'))
                | (accStake, (stake, relays)) <- accumulateBigLedgerStake peers
                , let relays' = NonEmpty.filter
                                  (\case
                                      LedgerRelayAccessSRVDomain {} -> True
                                      _ -> False
                                  )
                                  relays
                , not (null relays')
                ]
              pt' :: Point SomeHashableBlock
              pt' = Point $ getPoint pt <&>
                              \blk -> blk { blockPointSlot = maxBound }
              srvRelays = force
                [ (stake, NonEmpty.fromList relays')
                | (stake, relays) <- peers
                , let relays' = NonEmpty.filter
                                  (\case
                                      LedgerRelayAccessSRVDomain {} -> True
                                      _ -> False
                                  )
                                  relays
                , not (null relays')
                ]

          atomically do
            writeTMVar ledgerPeersVar $ LedgerAllPeerSnapshotV23 pt magic srvRelays
            writeTVar  ledgerBigPeersVar . Just $! LedgerBigPeerSnapshotV23 pt' magic bigSrvRelays

          pure $ release systemStart toNextEpoch

        handleLedgerPeers _ = error "handleLedgerPeers: impossible!"


    -- release, continue the loop in `idle`
    release :: SystemStart
            -> NominalDiffTime
            -> ClientStAcquired
                 (CardanoBlock crypto)
                 (Point (CardanoBlock crypto))
                 (Query (CardanoBlock crypto))
                 m
                 Void
    release systemStart toNextEpoch = SendMsgRelease do
      threadDelay $ min (realToFrac toNextEpoch) 86400 -- TODO fuzz this?
      idle $ Just systemStart


connectToCardanoNode :: Tracer IO TraceLocalStateQueryClient
                     -> Bool -- ^ use ledger peers
                     -> LocalSnocket
                     -> FilePath
                     -> NetworkMagic
                     -> NodeKernel crypto ntnAddr IO
                     -> IO (Either SomeException Void)
connectToCardanoNode tracer ledgerPeers localSnocket' snocketPath networkMagic nodeKernel =
  connectTo
   localSnocket'
   nullNetworkConnectTracers --debuggingNetworkConnectTracers
   (combineVersions
     [ simpleSingletonVersions
         version
         NodeToClientVersionData {
             networkMagic
           , query = False
         }
         \_version ->
           Mx.OuroborosApplication
             [ Mx.MiniProtocol
                 { miniProtocolNum = Mx.MiniProtocolNum 7
                 , miniProtocolStart = Mx.StartEagerly
                 , miniProtocolLimits =
                     Mx.MiniProtocolLimits
                       { maximumIngressQueue = 0xffffffff
                       }
                 , miniProtocolRun =
                     Mx.InitiatorProtocolOnly
                       . Mx.mkMiniProtocolCbFromPeerSt
                       . const
                       $ ( nullTracer -- TODO: add tracer
                         , cStateQueryCodec
                         , StateIdle
                         , localStateQueryClientPeer
                           $ cardanoClient tracer
                                           ledgerPeers
                                           (stakePools nodeKernel)
                                           (nextEpochVar nodeKernel)
                         )
                 }
             ]
     | version <- [minBound..maxBound]
     , let supportedVersionMap = supportedNodeToClientVersions (Proxy :: Proxy (CardanoBlock StandardCrypto))
           blk = supportedVersionMap Map.! version
           Codecs {cStateQueryCodec} =
             clientCodecs (pClientInfoCodecConfig . protocolClientInfoCardano $ EpochSlots 21600) blk version
     ])
   snocketPath
