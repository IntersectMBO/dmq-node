{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module DMQ.NodeToClient.LocalStateQueryClient
  ( Readiness (..)
  , CardanoLocalStateQueryClient
  , TraceLocalStateQueryClient (..)
  , cardanoLocalStateQueryClient
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (force)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Trans.Except
import "contra-tracer" Control.Tracer (Tracer, traceWith)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Void

import Cardano.Ledger.Api (EraGov)
import Cardano.Ledger.Api.State.Query (StakeSnapshots (..))
import Cardano.Network.PeerSelection (LedgerPeerSnapshot (..),
           LedgerRelayAccessPoint (..), SingLedgerPeersKind (..))
import Cardano.Slotting.EpochInfo.API
import Cardano.Slotting.Slot (EpochNo)
import Cardano.Slotting.Time

import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (mkEraMismatch)
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
           (QueryHardFork (GetCurrentEra, GetInterpreter))
import Ouroboros.Consensus.HardFork.History (Interpreter,
           interpreterToEpochInfo)
import Ouroboros.Consensus.Ledger.Query (Query (..), QueryFootprint (..))
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Query
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
import Ouroboros.Network.PeerSelection.LedgerPeers (LedgerPeersKind (..),
           accumulateBigLedgerStake)
import Ouroboros.Network.Protocol.LocalStateQuery.Client
import Ouroboros.Network.Protocol.LocalStateQuery.Type

import DMQ.Diffusion.NodeKernel.Types
import DMQ.NodeToClient.LocalStateQueryClient.Types


data QueryError = UnsupportedEra
  deriving Show

instance Exception QueryError where

data QueryInEra c proto era where
  QueryInEra ::
    EraGov era =>
    (forall result.
        BlockQuery (ShelleyBlock proto era) QFNoTables result
     -> CardanoQuery c QFNoTables (CardanoQueryResult c result))
    -> QueryInEra c proto era


--
-- Type aliases
--

-- | `LocalStateQuery` using `CardanoBlock`
type CardanoLocalStateQueryClient crypto m a =
   LocalStateQueryClient (CardanoBlock crypto)
                         (Point (CardanoBlock crypto))
                         (Query (CardanoBlock crypto)) m Void

-- | `ClientStIdle` state using `CardanoBlock`
type CardanoClientStIdle crypto m a =
    ClientStIdle (CardanoBlock crypto)
                 (Point (CardanoBlock crypto))
                 (Query (CardanoBlock crypto))
                 m a

-- | `ClientStAcuiring` using `CardanoBlock`
type CardanoClientStAcquiring crypto m a =
    ClientStAcquiring (CardanoBlock crypto)
                      (Point (CardanoBlock crypto))
                      (Query (CardanoBlock crypto))
                      m a

-- | `ClientStAcuired` using `CardanoBlock`
type CardanoClientStAcquired  crypto m a =
    ClientStAcquired (CardanoBlock crypto)
                     (Point (CardanoBlock crypto))
                     (Query (CardanoBlock crypto))
                     m a

-- | `ClientStQuerying` using `CardanoBlock`
type CardanoClientStQuerying crypto m a b =
    ClientStQuerying (CardanoBlock crypto)
                     (Point (CardanoBlock crypto))
                     (Query (CardanoBlock crypto))
                     m a b


-- | Local state query client which queries cardano node for
--
-- * stake pool data (for signature validation)
-- * ledger peers (for peer selection)
--
-- NOTE: we are querying using VolatileTip, e.g. for stake snapshot this
-- means that the mark set should not be trusted as it might be different
-- on different forks.
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
  -> StrictTVar m Readiness -- ^ from node kernel
  -> CardanoLocalStateQueryClient crypto m Void
cardanoLocalStateQueryClient tracer ledgerPeers
              StakePools {
                stakePoolsVar,
                ledgerPeersVar,
                ledgerBigPeersVar
              }
              readyVar = LocalStateQueryClient (idle Nothing)
  where
    idle :: Maybe SystemStart
         -> m (CardanoClientStIdle crypto m Void)
    idle mSystemStart = do
      traceWith tracer $ Acquiring mSystemStart
      pure $ SendMsgAcquire VolatileTip (acquire mSystemStart)

    acquire :: Maybe SystemStart
            -> CardanoClientStAcquiring crypto m Void
    acquire mSystemStart = ClientStAcquiring
      { recvMsgAcquired = maybe systemStartQuery eraQuery mSystemStart
      , recvMsgFailure = \failure ->
          throwIO . userError $ "recvMsgFailure: " <> show failure
      }

    systemStartQuery =
      pure . SendMsgQuery GetSystemStart $ ClientStQuerying eraQuery

    eraQuery systemStart = pure $
      SendMsgQuery (BlockQuery (QueryHardFork GetCurrentEra)) . ClientStQuerying $ \case
        EraByron{}    -> throwIO UnsupportedEra
        EraShelley{}  -> epochQuery systemStart (QueryInEra QueryIfCurrentShelley)
        EraAllegra{}  -> epochQuery systemStart (QueryInEra QueryIfCurrentAllegra)
        EraMary{}     -> epochQuery systemStart (QueryInEra QueryIfCurrentMary)
        EraAlonzo{}   -> epochQuery systemStart (QueryInEra QueryIfCurrentAlonzo)
        EraBabbage{}  -> epochQuery systemStart (QueryInEra QueryIfCurrentBabbage)
        EraConway{}   -> epochQuery systemStart (QueryInEra QueryIfCurrentConway)
        EraDijkstra{} -> epochQuery systemStart (QueryInEra QueryIfCurrentDijkstra)

    epochQuery :: SystemStart
               -> QueryInEra crypto proto era
               -> m (CardanoClientStAcquired crypto m Void)
    epochQuery systemStart qie@(QueryInEra f) =
        pure . SendMsgQuery (BlockQuery $ f GetEpochNo)
      $ wrappingMismatch (handleEpoch systemStart qie)


    wrappingMismatch :: forall r.
                        (r -> m (CardanoClientStAcquired crypto m Void))
                     -> CardanoClientStQuerying crypto m Void (CardanoQueryResult crypto r)
    wrappingMismatch k = ClientStQuerying $
      either (throwIO . userError . show . mkEraMismatch) k


    handleEpoch :: SystemStart
                -> QueryInEra crypto proto era
                -> EpochNo
                -> m (CardanoClientStAcquired crypto m Void)
    handleEpoch systemStart qie epoch =
      pure . SendMsgQuery (BlockQuery . QueryHardFork $ GetInterpreter)
       $ getInterpreter systemStart qie epoch


    getInterpreter :: SystemStart
                   -> QueryInEra crypto proto era
                   -> EpochNo
                   -> CardanoClientStQuerying crypto m Void (Interpreter xs)
    getInterpreter systemStart qie epoch =
      ClientStQuerying \interpreter -> do
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
            queryStakeSnapshots systemStart nextEpoch qie


    queryStakeSnapshots :: SystemStart
                        -> UTCTime
                        -> QueryInEra crypto proto era
                        -> m (CardanoClientStAcquired crypto m Void)
    queryStakeSnapshots systemStart nextEpoch qie@(QueryInEra f) =
      pure . SendMsgQuery (BlockQuery . f $ GetStakeSnapshots Nothing)
       $ wrappingMismatch \StakeSnapshots { ssStakeSnapshots } -> do
           atomically do
             writeTVar stakePoolsVar ssStakeSnapshots
             writeTVar readyVar Ready
           pure
             if ledgerPeers
               then
                 -- continue with ledger peers query
                 queryLedgerPeers systemStart nextEpoch qie
               else
                 -- release and continue in the idle state
                 release systemStart nextEpoch


    -- query ledger peer snapshot
    queryLedgerPeers
      :: SystemStart
      -> UTCTime
      -> QueryInEra crypto proto era
      -> CardanoClientStAcquired crypto m Void
    queryLedgerPeers systemStart nextEpoch (QueryInEra f) =
        SendMsgQuery (BlockQuery . f $ GetLedgerPeerSnapshot SingAllLedgerPeers)
        $ wrappingMismatch handleLedgerPeers
      where
        handleLedgerPeers
          :: LedgerPeerSnapshot AllLedgerPeers
          -> m (CardanoClientStAcquired crypto m Void)
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
            writeTVar  ledgerBigPeersVar . Just $! LedgerBigPeerSnapshotV23 pt magic bigSrvRelays

          pure $ release systemStart nextEpoch


    -- release, continue the loop in `idle`
    release :: SystemStart
            -> UTCTime
            -- ^ next epoch
            -> CardanoClientStAcquired crypto m Void
    release systemStart nextEpoch = SendMsgRelease do
      toNextEpoch <- diffUTCTime nextEpoch <$> getCurrentTime
      let toNextEpoch' :: DiffTime
          toNextEpoch' = max 1 (realToFrac toNextEpoch) `min` 86400
      traceWith tracer $ NextEpoch nextEpoch toNextEpoch'
      threadDelay toNextEpoch'
      idle $ Just systemStart
