{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.PeerSelection.PeerMetric
  ( ReportPeerMetrics' (..)
  , ReportPeerMetricsI
  , ReportPeerMetrics
  , hoist
  , nullMetrics
  , reportMetric
  , IsValid (..)
  , PeerMetricsConfiguration (..)
  , PeerMetrics
  , mkPeerMetrics
  , erasePeer
  , announciness
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import "contra-tracer" Control.Tracer
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity (Identity)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as OrdPSQ

import Network.Mux.Trace (TraceLabelPeer (..))


newtype PeerMetricsConfiguration = PeerMetricsConfiguration {
    timeWindowToKeep :: DiffTime
  }


-- | Internal state of the metric.
--
-- SigId enters `sigUnchecked` when we first hear about it from an upstream
-- peer.  It's evacuated to `sigMetric` when we acknowledge (it could be either
-- valid, non-valid or unknown - e.g. we didn't receive it).  If it was unknown
-- or invalid, it will be removed from `sigUnchecked` for that peer.
-- We only keep `timwWindowToKeep` oldest signatures in the state.
data PeerMetricsState sigid peeraddr = PeerMetricsState {
    -- | We store all `SigId`s received `Time` when each peer received it.  At
    -- this point we don't know yet if it's a valid signature.
    sigUnchecked :: Map sigid (OrdPSQ peeraddr Time ())

  , -- | When a `Sig` for a given `SigId` was fetched, we will move it from
    -- `sigUncheked` to `sigMetric`.  We can also remove all `peeraddr` which
    -- `Time` is greater than the received one, since none of them can win the
    -- race.  If `Sig` turned out to be invalid or we didn't receive it we
    -- remove it from `sigUnchecked`.
    sigMetric    :: OrdPSQ sigid Time peeraddr
  }

-- | Mutable peer metrics state accessible via 'STM'.
--
newtype PeerMetrics m sigid peeraddr = PeerMetrics {
    peerMetricsVar :: StrictTVar m (PeerMetricsState sigid peeraddr)
  }

mkPeerMetrics :: MonadSTM m => m (PeerMetrics m sigid peeraddr)
mkPeerMetrics =
  PeerMetrics <$>
  newTVarIO PeerMetricsState {
      sigUnchecked = Map.empty,
      sigMetric   = OrdPSQ.empty
    }

data IsValid =
    -- | Sig was valid
    Valid
  | -- | Sig was not valid or was not received when we requested it.
    NotValidOrNotReceived

data ReportPeerMetrics' m sigid f = ReportPeerMetrics {
    -- | report a new `sigid`
    reportSigId :: Tracer (STM m) (f (sigid, Time)),

    -- | report a received `sig`
    reportSig   :: Tracer (STM m) (f (sigid, IsValid))
  }

type ReportPeerMetricsI m sigid          = ReportPeerMetrics' m sigid Identity
type ReportPeerMetrics  m sigid peeraddr = ReportPeerMetrics' m sigid (TraceLabelPeer peeraddr)

hoist :: (forall a. f a -> g a)
      -> ReportPeerMetrics' m sigid g
      -> ReportPeerMetrics' m sigid f
hoist nat
      ReportPeerMetrics {
        reportSigId,
        reportSig
      }
      =
      ReportPeerMetrics {
        reportSigId = nat >$< reportSigId,
        reportSig   = nat >$< reportSig
      }


nullMetrics :: Applicative (STM m) => ReportPeerMetrics' m sigid f
nullMetrics = ReportPeerMetrics {
    reportSigId   = nullTracer,
    reportSig     = nullTracer
  }

reportMetric
  :: forall m sigid peeraddr.
     ( MonadSTM m
     , Ord sigid
     , Ord peeraddr
     )
   => PeerMetricsConfiguration
   -> PeerMetrics m sigid peeraddr
   -> ReportPeerMetrics m sigid peeraddr
reportMetric config peerMetrics =
  ReportPeerMetrics {
      reportSigId   = sigIdMetricTracer        peerMetrics,
      reportSig     = sigMetricTracer   config peerMetrics
    }


sigIdMetricTracer
  :: forall m sigid peeraddr.
     ( MonadSTM m
     , Ord sigid
     , Ord peeraddr
     )
  => PeerMetrics m sigid peeraddr
  -> Tracer (STM m) (TraceLabelPeer peeraddr (sigid, Time))
sigIdMetricTracer
    PeerMetrics { peerMetricsVar }
    =
    Tracer $ \(TraceLabelPeer peeraddr (sigid, time)) -> do
      st@PeerMetricsState { sigUnchecked } <- readTVar peerMetricsVar
      let sigUnchecked' = Map.alter (Just . fn peeraddr time) sigid sigUnchecked
      writeTVar peerMetricsVar st { sigUnchecked = sigUnchecked' }
  where
    fn :: peeraddr
       -> Time
       -> Maybe (OrdPSQ peeraddr Time ())
       -> OrdPSQ peeraddr Time ()
    fn peeraddr time Nothing    = OrdPSQ.singleton peeraddr time ()
    fn peeraddr time (Just psq) = snd $ OrdPSQ.alter gn peeraddr psq
      where
        gn :: Maybe (Time, ()) -> ((), Maybe (Time, ()))
        gn Nothing            = ((), Just (time, ()))
        -- we store only the earliest time from the given peer
        gn (Just (time', ())) = ((), Just (time' `min` time, ()))


sigMetricTracer
  :: forall m sigid peeraddr.
     ( MonadSTM m
     , Ord sigid
     , Ord peeraddr
     )
  => PeerMetricsConfiguration
  -> PeerMetrics m sigid peeraddr
  -> Tracer (STM m)
            (TraceLabelPeer peeraddr
                            ( sigid
                            , IsValid
                            ))
sigMetricTracer
    PeerMetricsConfiguration { timeWindowToKeep }
    PeerMetrics { peerMetricsVar }
    =
    Tracer $ \(TraceLabelPeer peeraddr (sigid, isValid)) -> do
      st@PeerMetricsState { sigUnchecked, sigMetric } <- readTVar peerMetricsVar
      case isValid of
        NotValidOrNotReceived -> do
          let notValidFn :: Maybe (OrdPSQ peeraddr Time v)
                         -> Maybe (OrdPSQ peeraddr Time v)
              notValidFn Nothing = Nothing
              notValidFn (Just psq) =
                -- delete the entry and make sure we don't store empty `psq` in
                -- the outer map
                let psq' = OrdPSQ.delete peeraddr psq
                in case OrdPSQ.findMin psq' of
                  Nothing -> Nothing
                  Just {} -> Just psq'
              sigUnchecked' = Map.alter notValidFn sigid sigUnchecked
          writeTVar peerMetricsVar st { sigUnchecked = sigUnchecked' }

        Valid -> do
          let validFn :: Maybe (OrdPSQ peeraddr Time ())
                      -> (Maybe Time, Maybe (OrdPSQ peeraddr Time ()))
              validFn Nothing = (Nothing, Nothing)
              validFn (Just psq) =
                -- delete the entry, return time and make sure we don't store
                -- empty `psq` in the outer map
                let (mbTime', psq') = OrdPSQ.alter (\case
                                            Nothing -> (Nothing, Nothing)
                                            Just (time, _) -> (Just time, Nothing)
                                        )
                                        peeraddr
                                        psq
                in case OrdPSQ.findMin psq' of
                  Nothing -> (mbTime', Nothing)
                  Just {} -> (mbTime', Just psq')
              (mbTime, sigUnchecked') = Map.alterF validFn sigid sigUnchecked

              sigMetric' = case mbTime of
                Nothing -> sigMetric
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
                  sigMetric

              sigMetric'' =
                case mbTime of
                  Nothing -> sigMetric'
                  Just t  -> snd $ OrdPSQ.atMostView ((- timeWindowToKeep) `addTime` t) sigMetric'

          writeTVar peerMetricsVar PeerMetricsState {
              sigUnchecked = sigUnchecked',
              sigMetric    = sigMetric''
            }


erasePeerImpl
  :: ( Ord peeraddr
     , Ord sigid
     )
  => peeraddr
  -> PeerMetricsState sigid peeraddr
  -> PeerMetricsState sigid peeraddr
erasePeerImpl
  peeraddr
  PeerMetricsState {
    sigUnchecked,
    sigMetric
  }
  =
  PeerMetricsState {
    sigUnchecked =
      Map.map
        (OrdPSQ.delete peeraddr)
        sigUnchecked,
    sigMetric =
        OrdPSQ.fromList
      . List.filter (\(_, _, peeraddr') -> peeraddr' /= peeraddr)
      . OrdPSQ.toAscList
      $ sigMetric
  }

-- | Erase a peer from `PeerMetric`.
--
erasePeer
  :: ( MonadSTM m
     , Ord peeraddr
     , Ord sigid
     )
  => peeraddr
  -> PeerMetrics m sigid peeraddr
  -> m ()
erasePeer peeraddr PeerMetrics { peerMetricsVar } =
  atomically $ modifyTVar peerMetricsVar (erasePeerImpl peeraddr)


announcinessImpl
  :: forall sigid peeraddr. Ord peeraddr
  => PeerMetricsState sigid peeraddr
  -> Map peeraddr Int
announcinessImpl PeerMetricsState { sigMetric }
  = OrdPSQ.fold' count Map.empty sigMetric
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
  => PeerMetrics m sigid peeraddr
  -> STM m (Map peeraddr Int)
announciness PeerMetrics { peerMetricsVar } =
    announcinessImpl <$> readTVar peerMetricsVar
