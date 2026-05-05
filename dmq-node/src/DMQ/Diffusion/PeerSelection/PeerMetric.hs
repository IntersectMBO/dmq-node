{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.Diffusion.PeerSelection.PeerMetric
  ( ReportPeerMetric' (..)
  , ReportPeerMetricI
  , ReportPeerMetric
  , hoist
  , nullMetrics
  , LocalPeerMetricState
  , emptyLocalPeerMetricState
  , reportMetric
  , PeerMetricConfiguration (..)
  , PeerMetric
  , mkPeerMetric
  , erasePeer
  , announciness
    -- * Re-exports
  , TraceLabelPeer (..)
  , TxMempoolResult (..)
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Data.Functor.Identity (Identity)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as OrdPSQ

import Network.Mux.Trace (TraceLabelPeer (..))

import Ouroboros.Network.TxSubmission.Inbound.V2 (TxMempoolResult (..))


newtype PeerMetricConfiguration = PeerMetricConfiguration {
    timeWindowToKeep :: DiffTime
  }

-- | Each peer keeps its own `LocalPeerMetricState`.  A `sigid` enters this
-- state when it is received, leaves when the corresponding signature is
-- downloaded.
--
newtype LocalPeerMetricState sigid = LocalPeerMetricState {
    localState :: OrdPSQ sigid Time ()
  }
  deriving Show

emptyLocalPeerMetricState
  :: LocalPeerMetricState sigid
emptyLocalPeerMetricState = LocalPeerMetricState { localState = OrdPSQ.empty }

reportSigIdImpl
  :: Ord sigid
  => PeerMetricConfiguration
  -> sigid
  -> Time
  -> LocalPeerMetricState sigid
  -> LocalPeerMetricState sigid
reportSigIdImpl
    PeerMetricConfiguration { timeWindowToKeep }
    sigid
    time
    LocalPeerMetricState { localState }
    =
    LocalPeerMetricState { localState = localState' }
  where
    localState' =
        snd
      . OrdPSQ.atMostView ((-timeWindowToKeep) `addTime` time)
        -- Unlike in `reportSigImpl` we overwrite the entry if it's already
        -- there, keeping the most recent announcement time.  This prevents a
        -- peer from announcing sigids early to bank a good timestamp and then
        -- sending the sigs only once they are forged.
      . OrdPSQ.insert sigid time ()
      $ localState


-- | Internal, used by `reportSig` to remove an entry from
-- `LocalPeerMetricState` and get the time when it entered it.
--
unreportSigId
  :: Ord sigid
  => sigid
  -> LocalPeerMetricState sigid
  -> (Maybe Time, LocalPeerMetricState sigid)
unreportSigId
    sigid
    LocalPeerMetricState { localState }
    =
    (mbTime, LocalPeerMetricState { localState = localState' })
  where
    (mbTime, localState') = OrdPSQ.alter fn sigid localState
    fn Nothing       = (Nothing, Nothing)
    fn (Just (t, _)) = (Just t,  Nothing)


-- | Internal state of the metric.
--
newtype PeerMetricState sigid peeraddr = PeerMetricState {
    metricState :: OrdPSQ sigid Time peeraddr
  }

instance (Show sigid, Show peeraddr)
      => Show (PeerMetricState sigid peeraddr) where
    show PeerMetricState { metricState } =
      unwords [ "PeerMetricState", show $ OrdPSQ.toAscList metricState ]

emptyPeerMetricState :: PeerMetricState sigid peeraddr
emptyPeerMetricState = PeerMetricState OrdPSQ.empty

-- | Mutable peer metrics state accessible via 'STM'.
--
newtype PeerMetric m sigid peeraddr = PeerMetric {
    peerMetricVar :: StrictTVar m (PeerMetricState sigid peeraddr)
  }

mkPeerMetric :: MonadSTM m
             => m (PeerMetric m sigid peeraddr)
mkPeerMetric =
  PeerMetric <$>
  newTVarIO emptyPeerMetricState

data ReportPeerMetric' m sigid f = ReportPeerMetric {
    -- | Report a received `sigid`
    reportSigId :: sigid
                -> Time
                -> LocalPeerMetricState sigid
                -> LocalPeerMetricState sigid,

    -- | Report a received `sig`
    reportSig :: LocalPeerMetricState sigid
              -> f (sigid, TxMempoolResult)
              -> STM m (LocalPeerMetricState sigid)
  }

type ReportPeerMetricI m sigid          = ReportPeerMetric' m sigid Identity
type ReportPeerMetric  m sigid peeraddr = ReportPeerMetric' m sigid (TraceLabelPeer peeraddr)

hoist
  :: (forall a. f a -> g a)
  -> ReportPeerMetric' m sigid g
  -> ReportPeerMetric' m sigid f
hoist
  nat
  ReportPeerMetric {
    reportSigId,
    reportSig
  }
  =
  ReportPeerMetric {
    reportSigId,
    reportSig = \localState -> reportSig localState . nat
  }


nullMetrics :: Applicative (STM m) => ReportPeerMetric' m sigid f
nullMetrics = ReportPeerMetric {
    reportSigId = \_ _ localState -> localState,
    reportSig   = \localState _ -> pure localState
  }


reportMetric
  :: forall m sigid peeraddr.
     ( MonadSTM m
     , Ord sigid
     )
   => PeerMetricConfiguration
   -> PeerMetric m sigid peeraddr
   -> ReportPeerMetric m sigid peeraddr
reportMetric config PeerMetric { peerMetricVar } =
  ReportPeerMetric {
    reportSigId = reportSigIdImpl config,
    reportSig = \localState ->
      stateTVar peerMetricVar
    . reportSigImpl config localState
  }


-- | An internal function which prunes the `PeerMetricState` keeping only
-- `timeWindowToKeep` of entries.
--
-- Note: we can trust the time, since it's coming from the host
-- `getMonotonicTime`.
prune :: Ord sigid
      => PeerMetricConfiguration
      -> Maybe Time
      -- ^ time when sigid was inserted to `LocalPeerMetricState`
      -> PeerMetricState sigid peeraddr
      -> PeerMetricState sigid peeraddr
prune PeerMetricConfiguration { timeWindowToKeep }
      mbTime
      PeerMetricState {
        metricState
      }
    = PeerMetricState {
        metricState =
          case mbTime of
            Nothing -> metricState
            Just t  -> snd $ OrdPSQ.atMostView ((-timeWindowToKeep) `addTime` t) metricState
      }


reportSigImpl
  :: forall sigid peeraddr.
     Ord sigid
  => PeerMetricConfiguration
  -> LocalPeerMetricState sigid
  -> TraceLabelPeer peeraddr (sigid, TxMempoolResult)
  -> PeerMetricState sigid peeraddr
  -> (LocalPeerMetricState sigid, PeerMetricState sigid peeraddr)
reportSigImpl
    config
    localState
    (TraceLabelPeer _peeraddr (sigid, TxRejected))
    metricState
    =
    ( localState'
    , prune config mbTime metricState
    )
  where
    localState' :: LocalPeerMetricState sigid
    (mbTime, localState') = unreportSigId sigid localState
reportSigImpl
    config
    localState
    (TraceLabelPeer peeraddr (sigid, TxAccepted))
    st
    =
    ( localState'
    , PeerMetricState { metricState = metricState' }
    )
  where
    mbTime      :: Maybe Time
    localState' :: LocalPeerMetricState sigid
    (mbTime, localState') = unreportSigId sigid localState

    -- First prune entries, then add a new one.  This is important in the edge
    -- case when there was an entry that is before the cut-off time, which we
    -- prune first then add the new result.  If we first added the new result then
    -- pruned: the time would be preserved and then pruned - at the end the
    -- current entry would be lost, while it shouldn't.
    PeerMetricState { metricState } = prune config mbTime st

    metricState' :: OrdPSQ sigid Time peeraddr
    metricState' = case mbTime of
      Nothing -> metricState
      Just time -> snd $ OrdPSQ.alter
        (\case
          Nothing -> ((), Just (time, peeraddr))
          Just a@(time', _) ->
            -- keep the earliest entry
            if time' < time
              then ((), Just a)
              else ((), Just (time, peeraddr))
        )
        sigid
        metricState


erasePeerImpl
  :: ( Ord peeraddr
     , Ord sigid
     )
  => peeraddr
  -> PeerMetricState sigid peeraddr
  -> PeerMetricState sigid peeraddr
erasePeerImpl
  peeraddr
  PeerMetricState {
    metricState
  }
  =
  PeerMetricState {
    metricState =
        -- O(n log n) filtering
        OrdPSQ.fromList
      . List.filter (\(_, _, peeraddr') -> peeraddr' /= peeraddr)
      . OrdPSQ.toAscList
      $ metricState
  }

-- | Erase a peer from `PeerMetric`.
--
erasePeer
  :: ( MonadSTM m
     , Ord peeraddr
     , Ord sigid
     )
  => peeraddr
  -> PeerMetric m sigid peeraddr
  -> m ()
erasePeer peeraddr PeerMetric { peerMetricVar } =
  atomically $ modifyTVar peerMetricVar (erasePeerImpl peeraddr)


announcinessImpl
  :: forall sigid peeraddr. Ord peeraddr
  => PeerMetricState sigid peeraddr
  -> Map peeraddr Int
announcinessImpl PeerMetricState { metricState }
  = OrdPSQ.fold' count Map.empty metricState
  where
    count :: sigid
          -> Time
          -> peeraddr
          -> Map peeraddr Int
          -> Map peeraddr Int
    count _ _ peeraddr m =
        Map.alter fn peeraddr m

    fn :: Maybe Int -> Maybe Int
    fn Nothing  = Just 1
    fn (Just n) = Just (n + 1)


-- | Metric counters.
--
announciness
  :: forall m sigid peeraddr.
     ( Ord peeraddr
     , MonadSTM m
     )
  => PeerMetric m sigid peeraddr
  -> STM m (Map peeraddr Int)
announciness PeerMetric { peerMetricVar } =
    announcinessImpl <$> readTVar peerMetricVar
