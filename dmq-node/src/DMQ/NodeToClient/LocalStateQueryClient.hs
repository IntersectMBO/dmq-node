{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PackageImports           #-}
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
import "contra-tracer" Control.Tracer (Tracer, traceWith)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Void

import Cardano.Ledger.Api.State.Query (StakeSnapshots (..))
import Cardano.Network.PeerSelection (LedgerPeerSnapshot (..),
           LedgerRelayAccessPoint (..), SingLedgerPeersKind (..))
import Cardano.Slotting.EpochInfo.API
import Cardano.Slotting.Time

import DMQ.Diffusion.NodeKernel.Types (StakePools (..))

import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import Ouroboros.Consensus.HardFork.History.EpochInfo (interpreterToEpochInfo)
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Shelley.Ledger.Query
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
import Ouroboros.Network.PeerSelection.LedgerPeers (LedgerPeersKind (..),
           RawBlockHash, accumulateBigLedgerStake)
import Ouroboros.Network.Point (Block (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Client
import Ouroboros.Network.Protocol.LocalStateQuery.Type

import DMQ.NodeToClient.LocalStateQueryClient.Types

data QueryError = UnsupportedEra
  deriving Show

instance Exception QueryError where

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

-- | `ClientStQuerying` using `CardanoBlock`
type CardanoClientStQuerying crypto m a b =
    ClientStQuerying (CardanoBlock crypto) (Point (CardanoBlock crypto)) (Query (CardanoBlock crypto)) m a b


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

    wrappingMismatch :: forall err r.
                        (r -> m (CardanoClientStAcquired crypto m Void))
                     -> CardanoClientStQuerying crypto m Void (Either err r)
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
          pure $ queryCurrentEra systemStart nextEpoch


    queryCurrentEra
      :: SystemStart
      -> UTCTime
      -> CardanoClientStAcquired crypto m Void
    queryCurrentEra systemStart nextEpoch =
        SendMsgQuery (BlockQuery (QueryHardFork GetCurrentEra))
          $ ClientStQuerying $ \era -> queryStakeSnapshots systemStart nextEpoch era

    -- query stake snapshot
    queryStakeSnapshots
      :: SystemStart
      -> UTCTime
      -> EraIndex (CardanoEras crypto)
      -> m (CardanoClientStAcquired crypto m Void)
    queryStakeSnapshots systemStart nextEpoch era =
        case era of
          EraByron{}    -> throwIO UnsupportedEra
          EraShelley{}  -> return $ SendMsgQuery (BlockQuery (QueryIfCurrentShelley (GetStakeSnapshots Nothing)))
                                  $ wrappingMismatch handleStakeSnapshots
          EraAllegra{}  -> return $ SendMsgQuery (BlockQuery (QueryIfCurrentAllegra (GetStakeSnapshots Nothing)))
                                  $ wrappingMismatch handleStakeSnapshots
          EraMary{}     -> return $ SendMsgQuery (BlockQuery (QueryIfCurrentMary (GetStakeSnapshots Nothing)))
                                  $ wrappingMismatch handleStakeSnapshots
          EraAlonzo{}   -> return $ SendMsgQuery (BlockQuery (QueryIfCurrentAlonzo (GetStakeSnapshots Nothing)))
                                  $ wrappingMismatch handleStakeSnapshots
          EraBabbage{}  -> return $ SendMsgQuery (BlockQuery (QueryIfCurrentBabbage (GetStakeSnapshots Nothing)))
                                  $ wrappingMismatch handleStakeSnapshots
          EraConway{}   -> return $ SendMsgQuery (BlockQuery (QueryIfCurrentConway (GetStakeSnapshots Nothing)))
                                  $ wrappingMismatch handleStakeSnapshots
          EraDijkstra{} -> return $ SendMsgQuery (BlockQuery (QueryIfCurrentDijkstra (GetStakeSnapshots Nothing)))
                                  $ wrappingMismatch handleStakeSnapshots
      where
        handleStakeSnapshots
          :: StakeSnapshots
          -> m (CardanoClientStAcquired crypto m Void)
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
        SendMsgQuery (BlockQuery . QueryIfCurrentConway $ GetLedgerPeerSnapshot SingAllLedgerPeers)
        $ wrappingMismatch handleLedgerPeers
      where
        handleLedgerPeers
          :: LedgerPeerSnapshot AllLedgerPeers
          -> m (ClientStAcquired
                  (CardanoBlock crypto)
                  (Point (CardanoBlock crypto))
                  (Query (CardanoBlock crypto))
                  m
                  Void)
        handleLedgerPeers (LedgerAllPeerSnapshotV23 pt magic peers) = do
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
              pt' :: Point RawBlockHash
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


    -- release, continue the loop in `idle`
    release :: SystemStart
            -> NominalDiffTime
            -> CardanoClientStAcquired crypto m Void
    release systemStart toNextEpoch = SendMsgRelease do
      threadDelay $ min (realToFrac toNextEpoch) 86400 -- TODO fuzz this?
      idle $ Just systemStart
