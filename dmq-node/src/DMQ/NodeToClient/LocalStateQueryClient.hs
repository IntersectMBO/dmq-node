{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}

module DMQ.NodeToClient.LocalStateQueryClient
  ( TraceLocalStateQueryClient (..)
  , CardanoLocalStateQueryClient
  , cardanoLocalStateQueryClient
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (force)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Trans.Except
import Control.Tracer (Tracer (..), traceWith)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Void

import Cardano.Ledger.Api.State.Query (StakeSnapshots (..))
import Cardano.Network.PeerSelection (LedgerPeerSnapshot (..),
           LedgerRelayAccessPoint (..))
import Cardano.Slotting.EpochInfo.API
import Cardano.Slotting.Slot (EpochNo)
import Cardano.Slotting.Time
import DMQ.Diffusion.NodeKernel.Types (StakePools (..))
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import Ouroboros.Consensus.HardFork.History.EpochInfo (interpreterToEpochInfo)
import Ouroboros.Consensus.HardFork.History.Qry (PastHorizonException)
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Shelley.Ledger.Query
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
import Ouroboros.Network.PeerSelection.LedgerPeers (LedgerPeersKind (..),
           SomeLedgerPeerSnapshot (..), accumulateBigLedgerStake)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (SomeHashableBlock)
import Ouroboros.Network.Point (Block (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Client
import Ouroboros.Network.Protocol.LocalStateQuery.Type

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

--
-- Type aliases
--

-- | `LocalStateQuery` using `CardanoBlock`
type CardanoLocalStateQueryClient crypto m a =
   LocalStateQueryClient (CardanoBlock crypto)
                         (Point (CardanoBlock crypto))
                         (Query (CardanoBlock crypto)) m Void

-- | `ClientStAcuiring` using `CardanoBlock`
type CardanoClientStAcquiring crypto m a =
    ClientStAcquiring (CardanoBlock crypto) (Point (CardanoBlock crypto)) (Query (CardanoBlock crypto)) m a

-- | `ClientStAcuired` using `CardanoBlock`
type CardanoClientStAcquired  crypto m a =
    ClientStAcquired (CardanoBlock crypto) (Point (CardanoBlock crypto)) (Query (CardanoBlock crypto)) m a


-- | Local state query client which queries cardano node for
--
-- * stake pool data (for signature validation)
-- * ledger peers (for peer selection)
--
-- TODO generalize to handle ledger eras other than Conway.
--
cardanoLocalStateQueryClient
  :: forall crypto m.
     ( MonadDelay m
     , MonadSTM m
     , MonadThrow m
     , MonadTime m
     )
  => Tracer m TraceLocalStateQueryClient
  -> Bool -- ^ use ledger peers
  -> StakePools m
  -> StrictTVar m (Maybe UTCTime) -- ^ from node kernel
  -> CardanoLocalStateQueryClient crypto m Void
cardanoLocalStateQueryClient
    tracer ledgerPeers
    StakePools {
      stakePoolsVar,
      ledgerPeersVar,
      ledgerBigPeersVar
    }
    nextEpochVar
    =
    LocalStateQueryClient (idle Nothing)
  where
    idle mSystemStart = do
      traceWith tracer $ Acquiring mSystemStart
      -- FIXME: switched to volatiletip for prerelease testing purposes
      pure $ SendMsgAcquire VolatileTip {-ImmutableTip-} acquire
      where
        acquire :: CardanoClientStAcquiring crypto m Void
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
      -> CardanoClientStAcquired crypto m Void
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
      -> CardanoClientStAcquired crypto m Void
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
            -> CardanoClientStAcquired crypto m Void
    release systemStart toNextEpoch = SendMsgRelease do
      threadDelay $ min (realToFrac toNextEpoch) 86400 -- TODO fuzz this?
      idle $ Just systemStart
