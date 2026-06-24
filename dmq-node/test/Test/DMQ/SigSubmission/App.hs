{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Test.DMQ.SigSubmission.App (tests) where

import Prelude hiding (seq)

import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Tracer (Tracer (..), contramap)
import System.Random (mkStdGen)

import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (toList, traverse_)
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.Functor.Identity (runIdentity)
import Data.Hashable
import Data.List (nubBy)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Typeable (Typeable)

import Network.TypedProtocol.Codec (AnyMessage (..))

import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Driver
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToReq (..))
import Ouroboros.Network.TxSubmission.Inbound.V2
import Ouroboros.Network.Util.ShowProxy

import DMQ.Diffusion.PeerSelection.PeerMetric (PeerMetricState (..),
           TraceLabelPeer (..), announcinessImpl, peerMetricVar)
import DMQ.Diffusion.PeerSelection.PeerMetric qualified as PeerMetric
import DMQ.Protocol.SigSubmissionV2.Codec (byteLimitsSigSubmissionV2,
           timeLimitsSigSubmissionV2)
import DMQ.Protocol.SigSubmissionV2.Inbound
           (sigSubmissionV2InboundPeerPipelined)
import DMQ.Protocol.SigSubmissionV2.Outbound (sigSubmissionV2OutboundPeer)
import DMQ.Protocol.SigSubmissionV2.Type (Message (..), NumIdsAck (..),
           SigSubmissionV2)
import DMQ.SigSubmissionV2.Inbound (sigSubmissionInbound)
import DMQ.SigSubmissionV2.Outbound (sigSubmissionOutbound)

import Test.DMQ.PeerSelection.PeerMetric ()
import Test.DMQ.SigSubmission.Types

import Test.Ouroboros.Network.Data.Signal ()
import Test.Ouroboros.Network.TxSubmission.Types
import Test.Ouroboros.Network.Utils hiding (debugTracer)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Test.DMQ.SigSubmissionV2.App"
  [ testProperty "sigSubmissionV2"        prop_sigSubmissionV2
  , testProperty "sigSubmissionV2_metric" prop_sigSubmissionV2_metric
  ]


-- | Tests overall sig submission semantics.
-- This property test is the same as for tx submission v1. We need this to know
-- we didn't regress.
--
prop_sigSubmissionV2 :: SigSubmissionState -> Property
prop_sigSubmissionV2 st@(SigSubmissionState peers _) =
    let tr = runSimTrace (sigSubmissionSimulation st)
        numPeersWithWronglySizedSig :: Int
        numPeersWithWronglySizedSig =
          foldr
            (\(sigs, _, _) r ->
              case List.find (\sig -> getTxSize sig /= getTxAdvSize sig) sigs of
                Just {} -> r + 1
                Nothing -> r
            ) 0 peers
    in
        label ("number of peers: " ++ renderRanges 3 (Map.size peers))
      . label ("number of sigs: "
              ++
              renderRanges 10
                ( Set.size
                . foldMap (Set.fromList . (\(sigs, _, _) -> getTxId <$> sigs))
                $ Map.elems peers
                ))
      . label ("number of peers with wrongly sized sig: "
             ++ show numPeersWithWronglySizedSig)
      $ case traceResult True tr of
         Left e ->
             counterexample (show e)
           . counterexample (ppTrace tr)
           $ False
         Right (inmp, outmps) ->
             counterexample (ppTrace tr)
           $ conjoin (validate inmp `map` outmps)
  where
    validate :: [Tx TxId] -- the inbound mempool
             -> [Tx TxId] -- one of the outbound mempools
             -> Property
    validate inmp outmp =
       let outUniqueSigIds = nubBy (on (==) getTxId) outmp
           outValidSigs    = filterValidSigs outmp
       in
       case ( length outUniqueSigIds == length outmp
            , length outValidSigs == length outmp
            ) of
         x@(True, True) ->
           -- If we are presented with a stream of unique sigids for valid
           -- signatures the inbound signatures should match the outbound
           -- signatures exactly.
             counterexample (show x)
           . counterexample (show inmp)
           . counterexample (show outmp)
           $ checkMempools inmp (take (length inmp) outValidSigs)

         x@(True, False) | Nothing <- List.find (\sig -> getTxAdvSize sig /= getTxSize sig) outmp  ->
           -- If we are presented with a stream of unique sigids then we should have
           -- fetched all valid signatures if all sigs have valid sizes.
             counterexample (show x)
           . counterexample (show inmp)
           . counterexample (show outValidSigs)

           $ checkMempools inmp (take (length inmp) outValidSigs)
                         | otherwise ->
             -- If there's one sig with an invalid size, we will download only
             -- some of them, but we don't guarantee how many we will download.
             --
             -- This is ok, the peer is cheating.
             property True


         x@(False, True) ->
           -- If we are presented with a stream of valid sigids then we should have
           -- fetched some version of those signatures.
             counterexample (show x)
           . counterexample (show inmp)
           . counterexample (show outmp)
           $ checkMempools (map getTxId inmp)
                           (take (length inmp)
                                 (getTxId <$> filterValidSigs outUniqueSigIds))

         (False, False) ->
           -- If we are presented with a stream of valid and invalid Sigs with
           -- duplicate sigids we're content with completing the protocol
           -- without error.
           property True


sigSubmissionSimulation :: forall s . SigSubmissionState
                        -> IOSim s ([Tx TxId], [[Tx TxId]])
                        -- ^ inbound & outbound mempools
sigSubmissionSimulation (SigSubmissionState state sigDecisionPolicy) = do
  state' <- traverse (\(sigs, mbOutDelay, mbInDelay) -> do
                      let mbOutDelayTime = getSmallDelay . getPositive <$> mbOutDelay
                          mbInDelayTime  = getSmallDelay . getPositive <$> mbInDelay
                      controlMessageVar <- newTVarIO Continue
                      return ( sigs
                             , controlMessageVar
                             , mbOutDelayTime
                             , mbInDelayTime
                             )
                    )
                    state

  state'' <- traverse (\(sigs, var, mbOutDelay, mbInDelay) -> do
                       return ( sigs
                              , readTVar var
                              , mbOutDelay
                              , mbInDelay
                              )
                    )
                    state'

  let simDelayTime = Map.foldl' (\m (sigs, _, mbInDelay, mbOutDelay) ->
                                  max m ( fromMaybe 1 (max <$> mbInDelay <*> mbOutDelay)
                                        * realToFrac (length sigs `div` 4)
                                        )
                                )
                                0
                                state''
      controlMessageVars = (\(_, x, _, _) -> x)
                        <$> Map.elems state'

  withAsync
    (do threadDelay (simDelayTime + 1100)
        atomically (traverse_ (`writeTVar` Terminate) controlMessageVars)
    ) \_ -> do
      let tracer :: forall a. (Show a, Typeable a) => Tracer (IOSim s) a
          tracer = dynamicTracer <> sayTracer
      runSigSubmissionV2 tracer tracer state'' sigDecisionPolicy


filterValidSigs :: [Tx TxId] -> [Tx TxId]
filterValidSigs
  = filter getTxValid
  . takeWhile (\Tx{getTxSize, getTxAdvSize} -> getTxSize == getTxAdvSize)


-- | Check that the inbound mempool contains all outbound `sig`s as a proper
-- subsequence.  It might contain more `sig`s from other peers.
--
checkMempools :: Eq sig
              => [sig] -- inbound mempool
              -> [sig] -- outbound mempool
              -> Bool
checkMempools _  []    = True  -- all outbound `sig` were found in the inbound
                               -- mempool
checkMempools [] (_:_) = False -- outbound mempool contains `sig`s which were
                               -- not transferred to the inbound mempool
checkMempools (i : is') os@(o : os')
  | i == o
  = checkMempools is' os'

  | otherwise
  -- `_i` is not present in the outbound mempool, we can skip it.
  = checkMempools is' os


runSigSubmissionV2
  :: forall m peeraddr sigid.
     ( MonadAsync       m
     , MonadDelay       m
     , MonadEvaluate    m
     , MonadFork        m
     , MonadLabelledSTM m
     , MonadMVar        m
     , MonadMask        m
     , MonadST          m
     , MonadSay         m
     , MonadThrow  (STM m)
     , MonadTime        m
     , MonadTimer       m
     , MonadTraceSTM    m
     , ShowProxy sigid
     , Typeable sigid
     , Show peeraddr
     , Ord peeraddr
     , Hashable peeraddr
     , Typeable peeraddr

     , sigid ~ Int
     )
  => Tracer m (String, TraceSendRecv (SigSubmissionV2 sigid (Tx sigid)))
  -> Tracer m (TraceTxLogic peeraddr sigid (Tx sigid))
  -> Map peeraddr ( [Tx sigid]
                  , ControlMessageSTM m
                  , Maybe DiffTime
                  , Maybe DiffTime
                  )
  -> TxDecisionPolicy
  -> m ([Tx sigid], [[Tx sigid]])
  -- ^ inbound and outbound mempools
runSigSubmissionV2 tracer tracerSigLogic st0 sigDecisionPolicy = do
    st <- traverse (\(b, c, d, e) -> do
        mempool <- newMempool b
        (outChannel, inChannel) <- createConnectedChannels
        return (mempool, c, d, e, outChannel, inChannel)
        ) st0
    inboundMempool <- emptyMempool
    let sigRng = mkStdGen 42 -- TODO

    sigChannelsVar <- newMVar (TxChannels Map.empty)
    sigMempoolSem <- newTxMempoolSem
    sharedSigStateVar <- newSharedTxStateVar sigRng
    traceTVarIO sharedSigStateVar \_ -> return . TraceDynamic . SigStateTrace
    labelTVarIO sharedSigStateVar "shared-sig-state"
    duplicateSigsVar <- LazySTM.newTVarIO []

    withAsync (decisionLogicThreads tracerSigLogic sayTracer
                                    sigDecisionPolicy sigChannelsVar sharedSigStateVar) $ \a -> do
      let outbounds = (\(addr, (mempool, _, outDelay, _, outChannel, _)) -> do
                      labelThisThread ("outbound-" ++ show addr)
                      let outbound = sigSubmissionOutbound
                                       (Tracer $ say . show)
                                       (NumIdsAck $ getNumTxIdsToReq $ maxUnacknowledgedTxIds sigDecisionPolicy)
                                       (getMempoolReader mempool)
                                       (maxBound :: TestVersion)
                      runPeerWithLimits
                        (("OUTBOUND " ++ show addr,) `contramap` tracer)
                        sigSubmissionCodec2
                        (byteLimitsSigSubmissionV2 (fromIntegral . BSL.length))
                        timeLimitsSigSubmissionV2
                        (maybe id delayChannel outDelay outChannel)
                        (sigSubmissionV2OutboundPeer outbound)
                    )
                   <$> Map.assocs st

      let inbounds = (\(addr, (_, ctrlMsgSTM, _, inDelay, _, inChannel)) -> do
                       labelThisThread ("inbound-" ++ show addr)
                       withPeer tracerSigLogic
                                sigChannelsVar
                                sigMempoolSem
                                sigDecisionPolicy
                                sharedSigStateVar
                                (getMempoolReader inboundMempool)
                                (getMempoolWriter duplicateSigsVar inboundMempool)
                                getTxSize
                                addr $ \(api :: PeerTxAPI m TxId (Tx TxId))-> do
                                  let inbound = sigSubmissionInbound
                                                  verboseTracer
                                                  (getMempoolWriter duplicateSigsVar inboundMempool)
                                                  api
                                                  PeerMetric.nullMetrics
                                                  ctrlMsgSTM
                                  runPipelinedPeerWithLimits
                                    (("INBOUND " ++ show addr,) `contramap` verboseTracer)
                                    sigSubmissionCodec2
                                    (byteLimitsSigSubmissionV2 (fromIntegral . BSL.length))
                                    timeLimitsSigSubmissionV2
                                    (maybe id delayChannel inDelay inChannel)
                                    (sigSubmissionV2InboundPeerPipelined inbound)
                    ) <$> Map.assocs st

      -- Run servers
      withAsyncAll (inbounds `zip` outbounds) $ \as -> do
        _ <- waitAllInbounds as
        -- cancel decision logic thread
        cancel a

        inmp <- readMempool inboundMempool
        let outmp = map (\(sigs, _, _, _) -> sigs)
                  $ Map.elems st0

        return (inmp, outmp)
  where
    waitAllInbounds :: [(Async m x, Async m x)] -> m [Either SomeException x]
    waitAllInbounds [] = return []
    waitAllInbounds ((inbound, outbound):as) = do
      r <- waitCatch inbound
      -- cancel outbound as soon as the inbound exits
      cancel outbound
      rs <- waitAllInbounds as
      return (r : rs)

    withAsyncAll :: [(m a, m a)]
                 -> ([(Async m a, Async m a)] -> m b)
                 -> m b
    withAsyncAll xs0 action = go [] xs0
      where
        go as []         = action (reverse as)
        go as ((x,y):xs) = withAsync x (\a -> withAsync y (\b -> go ((a, b):as) xs))


-- | The peer address type used in the simulation.  All trace-event type
-- aliases below are parameterised by this alias so that a change to the peer
-- address type causes a compile error rather than silently emptying the traces.
type SimPeerAddr = Int

-- | Dynamic-trace type for inbound protocol events (emitted by the per-peer
-- protocol tracer in 'runSigSubmissionV2WithMetric').
type SimProtocolEvent = TraceLabelPeer SimPeerAddr
                          (TraceSendRecv (SigSubmissionV2 TxId (Tx TxId)))

-- | Dynamic-trace type for inbound application-layer events (emitted by the
-- per-peer application tracer in 'runSigSubmissionV2WithMetric').
type SimAppEvent = TraceLabelPeer SimPeerAddr
                    (TraceTxSubmissionInbound TxId (Tx TxId))

-- | Dynamic-trace type for metric state snapshots (emitted by 'traceTVarIO'
-- on the peer-metric TVar in 'runSigSubmissionV2WithMetric').
type SimMetricSnapshot = PeerMetricState TxId SimPeerAddr

-- | Unified event type emitted into the dynamic trace so that peer events and
-- metric snapshots are extracted in a single ordered pass.  Using a single
-- 'selectTraceEventsDynamicWithTime' call preserves the in-thread interleaving
-- (sig_add → snapshot → sig_add → snapshot …) that separate calls would lose.
data SimTraceEvent
  = SimProtocolEvent  SimProtocolEvent
  | SimAppEvent       SimAppEvent
  | SimMetricSnapshot SimMetricSnapshot
  deriving Show


sigSubmissionSimulationWithMetric
  :: forall s.
     PeerMetric.PeerMetricConfiguration
  -> SigSubmissionState
  -> IOSim s ([Tx TxId], [[Tx TxId]])
sigSubmissionSimulationWithMetric config (SigSubmissionState state sigDecisionPolicy) = do
  state' <- traverse (\(sigs, mbOutDelay, mbInDelay) -> do
                       let mbOutDelayTime = getSmallDelay . getPositive <$> mbOutDelay
                           mbInDelayTime  = getSmallDelay . getPositive <$> mbInDelay
                       controlMessageVar <- newTVarIO Continue
                       return ( sigs
                              , controlMessageVar
                              , mbOutDelayTime
                              , mbInDelayTime
                              )
                      )
                      state

  state'' <- traverse (\(sigs, var, mbOutDelay, mbInDelay) ->
                        return ( sigs
                               , readTVar var
                               , mbOutDelay
                               , mbInDelay
                               )
                      )
                      state'

  let simDelayTime = Map.foldl' (\m (sigs, _, mbInDelay, mbOutDelay) ->
                                  max m ( fromMaybe 1 (max <$> mbInDelay <*> mbOutDelay)
                                        * realToFrac (length sigs `div` 4)
                                        )
                                )
                                0
                                state''
      controlMessageVars = (\(_, x, _, _) -> x)
                        <$> Map.elems state'

  withAsync
    (do threadDelay (simDelayTime + 1100)
        atomically (traverse_ (`writeTVar` Terminate) controlMessageVars)
    ) \_ -> do
      let tracer :: forall a. (Show a, Typeable a) => Tracer (IOSim s) a
          tracer = dynamicTracer <> sayTracer
      runSigSubmissionV2WithMetric tracer tracer config state'' sigDecisionPolicy


-- | Variant of 'runSigSubmissionV2' that wires a real 'PeerMetric' into each
-- inbound peer and registers a 'traceTVarIO' callback so that every
-- meaningful change to the metric state is emitted into the IOSim dynamic
-- trace as a 'SimMetricSnapshot'.
--
-- The inbound protocol tracer emits 'SimProtocolEvent' and the application
-- tracer emits 'SimAppEvent'; all three event kinds are consumed by
-- 'prop_sigSubmissionV2_metric'.
runSigSubmissionV2WithMetric
  :: forall s.
     Tracer (IOSim s) (String, TraceSendRecv (SigSubmissionV2 TxId (Tx TxId)))
  -> Tracer (IOSim s) (TraceTxLogic SimPeerAddr TxId (Tx TxId))
  -> PeerMetric.PeerMetricConfiguration
  -> Map SimPeerAddr ( [Tx TxId]
                     , ControlMessageSTM (IOSim s)
                     , Maybe DiffTime
                     , Maybe DiffTime
                     )
  -> TxDecisionPolicy
  -> IOSim s ([Tx TxId], [[Tx TxId]])
runSigSubmissionV2WithMetric tracer tracerSigLogic config st0 sigDecisionPolicy = do
    st <- traverse (\(b, c, d, e) -> do
        mempool <- newMempool b
        (outChannel, inChannel) <- createConnectedChannels
        return (mempool, c, d, e, outChannel, inChannel)
        ) st0
    inboundMempool <- emptyMempool
    let sigRng = mkStdGen 42

    sigChannelsVar <- newMVar (TxChannels Map.empty)
    sigMempoolSem <- newTxMempoolSem
    sharedSigStateVar <- newSharedTxStateVar sigRng
    traceTVarIO sharedSigStateVar \_ -> return . TraceDynamic . SigStateTrace
    labelTVarIO sharedSigStateVar "shared-sig-state"
    duplicateSigsVar <- LazySTM.newTVarIO []

    peerMetric <- PeerMetric.mkPeerMetric
    traceTVarIO (peerMetricVar peerMetric) $ \prev new ->
      pure $ case prev of
        Just p | PeerMetric.announcinessImpl p == PeerMetric.announcinessImpl new
               -> DontTrace
        _      -> TraceValue (Just (SimMetricSnapshot new)) (Just $ show new)

    withAsync (decisionLogicThreads tracerSigLogic sayTracer
                                    sigDecisionPolicy sigChannelsVar sharedSigStateVar) $ \a -> do
      let outbounds = (\(addr, (mempool, _, outDelay, _, outChannel, _)) -> do
                      labelThisThread ("outbound-" ++ show addr)
                      let outbound = sigSubmissionOutbound
                                       (Tracer $ say . show)
                                       (NumIdsAck $ getNumTxIdsToReq $ maxUnacknowledgedTxIds sigDecisionPolicy)
                                       (getMempoolReader mempool)
                                       (maxBound :: TestVersion)
                      runPeerWithLimits
                        (("OUTBOUND " ++ show addr,) `contramap` tracer)
                        sigSubmissionCodec2
                        (byteLimitsSigSubmissionV2 (fromIntegral . BSL.length))
                        timeLimitsSigSubmissionV2
                        (maybe id delayChannel outDelay outChannel)
                        (sigSubmissionV2OutboundPeer outbound)
                    )
                   <$> Map.assocs st

      let inbounds = (\(addr, (_, ctrlMsgSTM, _, inDelay, _, inChannel)) -> do
                       labelThisThread ("inbound-" ++ show addr)
                       withPeer tracerSigLogic
                                sigChannelsVar
                                sigMempoolSem
                                sigDecisionPolicy
                                sharedSigStateVar
                                (getMempoolReader inboundMempool)
                                (getMempoolWriter duplicateSigsVar inboundMempool)
                                getTxSize
                                addr $ \(api :: PeerTxAPI (IOSim s) TxId (Tx TxId)) -> do
                                  let inbound = sigSubmissionInbound
                                                  (contramap (SimAppEvent . TraceLabelPeer addr)
                                                    (dynamicTracer <> sayTracer :: Tracer (IOSim s) SimTraceEvent))
                                                  (getMempoolWriter duplicateSigsVar inboundMempool)
                                                  api
                                                  (PeerMetric.hoist
                                                    (TraceLabelPeer addr . runIdentity)
                                                    (PeerMetric.reportMetric
                                                      config
                                                      peerMetric))
                                                  ctrlMsgSTM
                                  runPipelinedPeerWithLimits
                                    (contramap (SimProtocolEvent . TraceLabelPeer addr)
                                      (dynamicTracer <> sayTracer :: Tracer (IOSim s) SimTraceEvent))
                                    sigSubmissionCodec2
                                    (byteLimitsSigSubmissionV2 (fromIntegral . BSL.length))
                                    timeLimitsSigSubmissionV2
                                    (maybe id delayChannel inDelay inChannel)
                                    (sigSubmissionV2InboundPeerPipelined inbound)
                    ) <$> Map.assocs st

      withAsyncAll (inbounds `zip` outbounds) $ \as -> do
        _ <- waitAllInbounds as
        cancel a

        inmp <- readMempool inboundMempool
        let outmp = map (\(sigs, _, _, _) -> sigs)
                  $ Map.elems st0

        return (inmp, outmp)
  where
    waitAllInbounds :: [(Async (IOSim s) x, Async (IOSim s) x)] -> IOSim s [Either SomeException x]
    waitAllInbounds [] = return []
    waitAllInbounds ((inbound, outbound):as) = do
      r <- waitCatch inbound
      cancel outbound
      rs <- waitAllInbounds as
      return (r : rs)

    withAsyncAll :: [(IOSim s a, IOSim s a)]
                 -> ([(Async (IOSim s) a, Async (IOSim s) a)] -> IOSim s b)
                 -> IOSim s b
    withAsyncAll xs0 action = go [] xs0
      where
        go as []         = action (reverse as)
        go as ((x,y):xs) = withAsync x (\a -> withAsync y (\b -> go ((a, b):as) xs))


-- | Checks that on every 'SimMetricSnapshot' in the merged trace the actual
-- 'announcinessImpl' agrees with the pure model rebuilt from 'SimProtocolEvent'
-- and 'SimAppEvent' events seen so far.
prop_sigSubmissionV2_metric :: PeerMetric.PeerMetricConfiguration
                            -> SigSubmissionState
                            -> Property
prop_sigSubmissionV2_metric config st@(SigSubmissionState peers _) =
    let tr = runSimTrace (sigSubmissionSimulationWithMetric config st)
    in  label ("number of peers: " ++ renderRanges 3 (Map.size peers))
      $ counterexample (ppTrace tr)
      $ case traceResult True tr of
          Left e ->
              counterexample (show e)
            . counterexample (ppTrace tr)
            $ False
          Right _ ->
            let events       = selectTraceEventsDynamicWithTime @_ @SimTraceEvent tr
                numSnapshots = length
                             . filter
                                 (\case
                                   (_, SimMetricSnapshot{}) -> True
                                   _ -> False
                                 )
                             $ events
                numAppEvents = length
                             . filter
                                 (\case
                                   (_, SimAppEvent{}) -> True
                                   _ -> False
                                 )
                             $ events
                anyNonEmpty  = any
                                (\case
                                  (_, SimMetricSnapshot sn) ->
                                        not (Map.null (announcinessImpl sn))
                                  _ -> False
                                )
                                events
            in  classify (numSnapshots > 0) "metric fired"
              . classify anyNonEmpty        "non-empty announciness observed"
              . label ("snapshots: "    ++ renderRanges  10 numSnapshots)
              . label ("mempool adds: " ++ renderRanges 100 numAppEvents)
              $ checkTrace config events


-- ---------------------------------------------------------------------------
-- Pure model
-- ---------------------------------------------------------------------------

-- | State of the pure model, mirroring the two-stage bookkeeping inside
-- 'reportSigImpl': first we record when each peer announced each sigid, then
-- on acceptance we move the entry into the accepted window.
data PureModelState = PureModelState
  { announced :: Map (TxId, SimPeerAddr) Time
    -- ^ (sigid, peer) → time the peer first announced the sigid
  , accepted  :: Map TxId (Time, SimPeerAddr)
    -- ^ accepted sigs still within the time window: sigid → (time, peer)
  }

emptyPureModelState :: PureModelState
emptyPureModelState = PureModelState Map.empty Map.empty

-- | Advance the pure model when a sigid is announced using a protocol message.
-- Only 'TraceRecvMsg' of 'MsgReplySigIds' matters: it records the IOSim time at
-- which the inbound peer received the sigid announcement.
--
-- Mirrors 'reportSigIdsImpl': prune stale per-peer announced entries (those
-- older than @timeWindowToKeep@ relative to the new announcement time) before
-- inserting the fresh ones.  Without this, the model diverges from production
-- when a channel delay pushes the gap between two consecutive announcements
-- for the same peer beyond the window.
--
updatePureModelOnSigAnnounced
  :: PeerMetric.PeerMetricConfiguration
  -> Time
  -> SimProtocolEvent
  -> PureModelState
  -> PureModelState
updatePureModelOnSigAnnounced
    (PeerMetric.PeerMetricConfiguration window)
    t
    (TraceLabelPeer addr (TraceRecvMsg (AnyMessage msg)))
    st =
  case msg of
    MsgReplySigIds sigids ->
      -- Mirror reportSigIdsImpl: pruning only happens when reportSigIds is
      -- called.
      let sigidsLst = fst <$> toList sigids
          threshold  = (-window) `addTime` t
          announced' = Map.filterWithKey
                         (\(_, a) tann -> a /= addr || tann > threshold)
                         (announced st)
      in  st { announced = Foldable.foldl' (\m txid -> Map.insert (txid, addr) t m)
                                           announced' sigidsLst }
    _ -> st
updatePureModelOnSigAnnounced _ _ _ st = st


-- | Advance the pure model when on mempool submission result.  Only
-- 'TraceTxInboundAddedToMempool' matters: it moves the sigid from the
-- announced set into the accepted window.
--
-- Note: the model is simple but good enough to provide the same announciness
-- metrics as the production implementation.  For example we never evacuate
-- entries from `announced`, but we make sure we insert and prune `accepted`
-- map in a way to make the mode faithful.
--
updatePureModelOnMempoolResult
  :: PeerMetric.PeerMetricConfiguration
  -> SimAppEvent
  -> PureModelState
  -> PureModelState

-- valid signatures:
-- * prune old entries
-- * insert new entry
updatePureModelOnMempoolResult
    (PeerMetric.PeerMetricConfiguration window)
    (TraceLabelPeer addr (TraceTxInboundAddedToMempool sigids _))
    st0
    =
    Foldable.foldl' fn st0 sigids
  where
    fn st sigid =
      case Map.lookup (sigid, addr) (announced st) of
        Nothing -> st
        Just t  ->
          let accepted'  = Map.filter (\(time, _) -> time > (-window) `addTime` t) (accepted st)
              accepted'' = Map.insert sigid (t, addr) accepted'
          in  st { accepted = accepted'' }

-- invalid signatures:
-- * prune old entries
updatePureModelOnMempoolResult
    (PeerMetric.PeerMetricConfiguration window)
    (TraceLabelPeer addr (TraceTxInboundRejectedFromMempool sigids _))
    st0
    =
    Foldable.foldl' fn st0 sigids
  where
    fn st sigid =
      case Map.lookup (sigid, addr) (announced st) of
        Nothing -> st
        Just t  ->
          let accepted' = Map.filter (\(time, _) -> time > (-window) `addTime` t) (accepted st)
          in  st { accepted = accepted' }

-- any other `TraceTxSubmissionInbound` event can be ignored
updatePureModelOnMempoolResult _ _ st = st


-- | Derive announciness counts from the pure model state.
--
expectedAnnounciness :: PureModelState -> Map SimPeerAddr Int
expectedAnnounciness PureModelState { accepted } =
  Map.fromListWith (+) [ (peer, 1) | (_, peer) <- Map.elems accepted ]


-- ---------------------------------------------------------------------------
-- Trace checking
-- ---------------------------------------------------------------------------

-- | Fold over the ordered event stream, updating the pure model on peer events
-- and asserting agreement on metric snapshots.
checkTrace
  :: PeerMetric.PeerMetricConfiguration
  -> [(Time, SimTraceEvent)]
  -> Property
checkTrace config evs =
    case Foldable.foldl' step (emptyPureModelState, property True, 0) evs of
      (_, prop, maxScore) ->
        label ("max score: " ++ renderRanges 2 maxScore) prop
  where
    step :: (PureModelState, Property, Int)
         -> (Time, SimTraceEvent)
         -> (PureModelState, Property, Int)
    step (st, prop, maxScore) (t, SimProtocolEvent event)  = (updatePureModelOnSigAnnounced config t event st, prop, maxScore)
    step (st, prop, maxScore) (_, SimAppEvent event)       = (updatePureModelOnMempoolResult config event st, prop, maxScore)
    step (st, prop, maxScore) (t, SimMetricSnapshot snapshot) =
      let expected  = expectedAnnounciness st
          actual    = announcinessImpl snapshot
          maxScore' = if Map.null actual then maxScore else maximum actual
      in  ( st
          , prop .&&. ( counterexample ("property violated at " ++ show t)
                      . counterexample ("actual:   " ++ show actual)
                      . counterexample ("expected: " ++ show expected)
                      $ actual === expected
                      )
          , maxScore'
          )
