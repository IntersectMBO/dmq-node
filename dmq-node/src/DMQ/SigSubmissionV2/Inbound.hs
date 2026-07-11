{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.SigSubmissionV2.Inbound
  ( -- * SigSubmision Inbound client
    sigSubmissionInbound
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Typeable (Typeable)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (foldM, unless)
import Control.Monad.Class.MonadAsync (MonadAsync (..))
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import "contra-tracer" Control.Tracer (Tracer, traceWith)

import Network.TypedProtocol

import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM,
           timeoutWithControlMessage)
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..),
           NumTxIdsToReq (..))
import Ouroboros.Network.RegisteredDelay qualified as RegisteredDelay
import Ouroboros.Network.TxSubmission.Inbound.V2 (PeerTxAPI (..),
           TxDecisionPolicy)
import Ouroboros.Network.TxSubmission.Inbound.V2.State qualified as State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types (PeerAction (..),
           PeerPhase (..), PeerTxLocalState (..), ProcessedTxCount (..),
           TraceTxSubmissionInbound (..), TxKey, TxSubmissionCounters (..),
           TxSubmissionMempoolWriter (..), TxSubmissionProtocolError (..),
           const_MAX_TX_SIZE_DISCREPANCY, diffTimeToMilliseconds,
           emptyPeerTxLocalState)

import DMQ.Diffusion.PeerSelection.PeerMetric (LocalPeerMetricState,
           ReportPeerMetric' (..), ReportPeerMetricI, SigMempoolResult (..))
import DMQ.Diffusion.PeerSelection.PeerMetric qualified as PeerMetric
import DMQ.Protocol.SigSubmissionV2.Inbound
import DMQ.Protocol.SigSubmissionV2.Type (NumIdsAck (..), NumIdsReq (..),
           SizeInBytes)
import DMQ.SigSubmissionV2.Types


-- | A sig-submission inbound side (client, sic!).
--
--
sigSubmissionInbound
  :: forall sigid sig idx m failure.
     ( MonadAsync m
     , MonadThrow m
     , MonadTimer m
     , Ord sigid
     , Show sigid
     , Typeable sigid
     )
  => Tracer m (TraceTxSubmissionInbound sigid sig)
  -> TxDecisionPolicy
  -> TxSubmissionMempoolWriter sigid sig idx m failure
  -> (sig -> SizeInBytes)
  -> PeerTxAPI m sigid sig
  -> ReportPeerMetricI m sigid
  -> ControlMessageSTM m
  -> SigSubmissionInboundPipelined sigid sig m ()
sigSubmissionInbound
    tracer
    policy
    TxSubmissionMempoolWriter { txId, mempoolAddTxs }
    sigSize
    PeerTxAPI {
      awaitSharedChange,
      runNextPeerAction,
      runNextPeerActionPipelined,
      applyReceivedTxIds,
      applyReceivedTxs,
      applySubmittedTxs,
      resolveTxRequest,
      resolveBufferedTxs,
      addCounters
    }
    ReportPeerMetric {
      reportSigIds,
      reportSig
    }
    controlMessageSTM
    =
    SigSubmissionInboundPipelined
      (serverIdle emptyPeerTxLocalState PeerMetric.emptyLocalPeerMetricState)
  where
    -- Entry point / reset state of the non-pipelined loop.  Called on start,
    -- when the pipeline drains, and after an idle peer wakes up.
    serverIdle :: PeerTxLocalState sig
               -> LocalPeerMetricState sigid
               -> m (InboundStIdle Z sigid sig m ())
    serverIdle peerState metricState = do
      -- non-blocking check for termination
      atomically controlMessageSTM >>= \case
        Terminate -> pure (SendMsgDone (return ()))
        _         -> do
          now <- getMonotonicTime
          -- when the pipeline fully drained, emit the body-download episode
          -- duration
          peerState' <- case peerDownloadStartTime peerState of
                             Nothing        -> pure peerState
                             Just startTime -> do
                               addCounters mempty {
                                   txPipelineWaitMs =
                                     diffTimeToMilliseconds (now `diffTime` startTime)
                                 }
                               pure peerState { peerDownloadStartTime = Nothing }
          -- traceCanRequest Zero peerState'
          (peerAction, peerState'') <-
            runNextPeerAction now (State.drainPeerScore policy now peerState')
          case peerAction of
            PeerDoNothing generation mDelay -> do
              -- An Active->Idle transition means this peer has just become
              -- eligible for actions it could not take before (e.g. claiming
              -- expired leases as an idle claimant). Re-run the scheduler
              -- immediately rather than parking on a wake condition that may
              -- not fire.
              let cameToIdle = peerPhase peerState'  /= PeerIdle
                            && peerPhase peerState'' == PeerIdle
              if cameToIdle
                then serverIdle peerState'' metricState
                else do
                  mRegisteredDelay <- traverse RegisteredDelay.new mDelay
                  timeoutWithControlMessage controlMessageSTM (awaitSharedChange generation mRegisteredDelay) >>= \case
                     Nothing -> pure (SendMsgDone (return ()))
                     Just {} -> serverIdle peerState'' metricState
            PeerSubmitTxs txKeys ->
              submitBufferedSigs txKeys peerState'' metricState serverIdle
            PeerRequestTxs txKeys ->
              requestSigBodies Zero txKeys peerState'' metricState
            PeerRequestTxIds txIdsToAck txIdsToReq ->
              serverReqSigIds Zero txIdsToAck txIdsToReq peerState'' metricState

    -- Submit buffered sig bodies to the mempool, feed the announciness metric,
    -- and continue with @k@.
    submitBufferedSigs :: forall (n :: N).
                          [TxKey]
                       -> PeerTxLocalState sig
                       -> LocalPeerMetricState sigid
                       -> (PeerTxLocalState sig -> LocalPeerMetricState sigid
                           -> m (InboundStIdle n sigid sig m ()))
                       -> m (InboundStIdle n sigid sig m ())
    submitBufferedSigs txKeys peerState metricState k = do
      bufferedSigs <- resolveBufferedTxs peerState txKeys

      start <- getMonotonicTime
      let submitted = [ (txKey, sigid') | (txKey, sigid', _) <- bufferedSigs ]
          toSubmit  = [ sig | (_, _, sig) <- bufferedSigs ]
      (acceptedSigIds, rejectedSigs) <- if null toSubmit
                                           then pure ([], [])
                                           else mempoolAddTxs toSubmit
      end <- getMonotonicTime

      -- 'mempoolAddTxs' partitions the batch into accepted/rejected; map each
      -- verdict's sigids back to our 'TxKey's via the submitted batch.
      let submittedKeyOf   = Map.fromList [ (sigid', txKey) | (txKey, sigid') <- submitted ]
          resolvedKeys     = mapMaybe (`Map.lookup` submittedKeyOf) acceptedSigIds
          rejectedKeys     = mapMaybe ((`Map.lookup` submittedKeyOf) . fst) rejectedSigs
          rejectedForTrace = fmap fst rejectedSigs
          acceptedCount    = length acceptedSigIds
          rejectedCount    = length rejectedSigs
          delta            = end `diffTime` start

      addCounters mempty { txSubmissionWaitMs = diffTimeToMilliseconds delta }
      peerState' <- applySubmittedTxs end resolvedKeys rejectedKeys peerState
      let (score, peerState'') =
            State.applyPeerEvents policy end acceptedCount rejectedCount peerState'
      traceWith tracer $
        TraceTxSubmissionProcessed ProcessedTxCount {
            ptxcAccepted = acceptedCount,
            ptxcRejected = rejectedCount,
            ptxcScore    = score
          }
      unless (null acceptedSigIds) $
        traceWith tracer (TraceTxInboundAddedToMempool acceptedSigIds delta)
      unless (null rejectedForTrace) $
        traceWith tracer (TraceTxInboundRejectedFromMempool rejectedForTrace delta)

      -- Bridge the batch verdict into DMQ's per-sig announciness metric.
      metricState'' <- atomically $ do
        metricState'
          <- foldM (\st sigid' -> reportSig st (Identity (sigid', SigAccepted)))
                   metricState acceptedSigIds
        let rejectedSigIds = fst <$> rejectedSigs
        foldM (\st sigid' -> reportSig st (Identity (sigid', SigRejected)))
              metricState' rejectedSigIds
      k peerState'' metricState''

    -- Send a pipelined request for sig bodies.
    requestSigBodies :: forall (n :: N).
                        Nat n
                     -> [TxKey]
                     -> PeerTxLocalState sig
                     -> LocalPeerMetricState sigid
                     -> m (InboundStIdle n sigid sig m ())
    requestSigBodies n txKeys peerState metricState = do
      sigsToRequest <- resolveTxRequest peerState txKeys
      traceWith tracer (TraceTxInboundRequestTxs (Map.keys sigsToRequest))

      -- Record the start of the download episode on the first outstanding
      -- body request.  Subsequent pipelined requests leave the start time
      -- unchanged so we measure from first-send to last-receive.
      sendTime <- getMonotonicTime
      let peerState' = case peerDownloadStartTime peerState of
                            Nothing -> peerState { peerDownloadStartTime = Just sendTime }
                            Just _  -> peerState
      pure $ SendMsgRequestSigsPipelined sigsToRequest
               (continueAfterBodyRequests (Succ n) peerState' metricState)

    -- Post-reply scheduling; dispatch the next action or drain the pipeline.
    continueAfterReplies :: forall (n :: N).
                            Nat n
                         -> PeerTxLocalState sig
                         -> LocalPeerMetricState sigid
                         -> m (InboundStIdle n sigid sig m ())
    continueAfterReplies Zero peerState metricState = serverIdle peerState metricState
    continueAfterReplies n@Succ{} peerState metricState =
      pipelinedDispatch n peerState metricState

    -- Post-body-request scheduling; same dispatch, guaranteed pipelined depth.
    continueAfterBodyRequests :: forall (n :: N).
                                 Nat (S n)
                              -> PeerTxLocalState sig
                              -> LocalPeerMetricState sigid
                              -> m (InboundStIdle (S n) sigid sig m ())
    continueAfterBodyRequests = pipelinedDispatch

    pipelinedDispatch :: forall (n :: N).
                         Nat (S n)
                      -> PeerTxLocalState sig
                      -> LocalPeerMetricState sigid
                      -> m (InboundStIdle (S n) sigid sig m ())
    pipelinedDispatch n peerState metricState = do
      now <- getMonotonicTime
      (peerAction, peerState') <-
        runNextPeerActionPipelined now (State.drainPeerScore policy now peerState)
      case peerAction of
        PeerSubmitTxs txKeys ->
          submitBufferedSigs txKeys peerState' metricState (continueAfterReplies n)
        PeerRequestTxs txKeys ->
          requestSigBodies n txKeys peerState' metricState
        PeerRequestTxIds txIdsToAck txIdsToReq ->
          serverReqSigIds n txIdsToAck txIdsToReq peerState' metricState
        PeerDoNothing{} ->
          pure $ handleReplies n peerState' metricState

    -- Construct and send a sigid request (blocking or pipelined).
    serverReqSigIds :: forall (n :: N).
                       Nat n
                    -> NumTxIdsToAck
                    -> NumTxIdsToReq
                    -> PeerTxLocalState sig
                    -> LocalPeerMetricState sigid
                    -> m (InboundStIdle n sigid sig m ())
    -- nothing to ack or request: back to idle
    serverReqSigIds Zero 0 0 peerState metricState =
      serverIdle peerState metricState

    -- nothing to do but pipeline not empty: drain remaining replies
    serverReqSigIds n@Succ{} 0 0 peerState metricState =
      pure $ handleReplies n peerState metricState

    -- non-pipelined; may send a blocking request
    serverReqSigIds Zero txIdsToAck txIdsToReq peerState metricState =
      if StrictSeq.null (peerUnacknowledgedTxIds peerState)
        then do
          sendTime <- getMonotonicTime
          addCounters mempty { txIdBlockingReqsSent = 1 }
          pure $ SendMsgRequestSigIdsBlocking
                   (NumIdsAck (getNumTxIdsToAck txIdsToAck))
                   (NumIdsReq (getNumTxIdsToReq txIdsToReq))
                   (\sigids -> do
                       now <- getMonotonicTime
                       addCounters mempty {
                           txIdBlockingWaitMs =
                             diffTimeToMilliseconds (now `diffTime` sendTime)
                         }
                       unless (length sigids <= fromIntegral txIdsToReq) $
                         throwIO ProtocolErrorSigIdsNotRequested
                       let metricState' = reportSigIds (fst <$> sigids) now metricState
                       peerState' <- applyReceivedTxIds now txIdsToReq sigids peerState
                       serverIdle peerState' metricState')
        else do
          addCounters mempty { txIdPipelinedReqsSent = 1 }
          pure $ SendMsgRequestSigIdsPipelined
                   (NumIdsAck (getNumTxIdsToAck txIdsToAck))
                   (NumIdsReq (getNumTxIdsToReq txIdsToReq))
                   (pure $ handleReplies (Succ Zero) peerState metricState)

    -- pipelined request at depth > 0
    serverReqSigIds n@Succ{} txIdsToAck txIdsToReq peerState metricState = do
      addCounters mempty { txIdPipelinedReqsSent = 1 }
      pure $ SendMsgRequestSigIdsPipelined
               (NumIdsAck (getNumTxIdsToAck txIdsToAck))
               (NumIdsReq (getNumTxIdsToReq txIdsToReq))
               (pure $ handleReplies (Succ n) peerState metricState)

    -- Prepare to collect a single pipelined reply.
    handleReplies :: forall (n :: N).
                     Nat (S n)
                  -> PeerTxLocalState sig
                  -> LocalPeerMetricState sigid
                  -> InboundStIdle (S n) sigid sig m ()
    handleReplies (Succ n) peerState metricState =
      CollectPipelined Nothing (handleReply n peerState metricState)

    -- Process a single pipelined reply.
    handleReply :: forall (n :: N).
                   Nat n
                -> PeerTxLocalState sig
                -> LocalPeerMetricState sigid
                -> Collect sigid sig
                -> m (InboundStIdle n sigid sig m ())
    handleReply n peerState metricState = \case
      CollectSigIds sigIdsToReq sigids -> do
        unless (length sigids <= fromIntegral (getNumIdsReq sigIdsToReq)) $
          throwIO ProtocolErrorSigIdsNotRequested
        now <- getMonotonicTime
        let metricState' = reportSigIds (fst <$> sigids) now metricState
        peerState' <-
          applyReceivedTxIds now
            (NumTxIdsToReq (getNumIdsReq sigIdsToReq)) sigids peerState
        continueAfterReplies n peerState' metricState'

      CollectSigs requested sigs -> do
        let received       = Map.fromList [ (txId sig, sig) | sig <- sigs ]
            wrongSizedSigs = collectWrongSizedSigs requested received
        unless (Map.keysSet received `Set.isSubsetOf` Map.keysSet requested) $
          throwIO ProtocolErrorSigNotRequested
        traceWith tracer $ TraceTxSubmissionCollected (txId <$> sigs)
        unless (null wrongSizedSigs) $ do
          let protocolError = ProtocolErrorTxSizeError wrongSizedSigs
          traceWith tracer (TraceTxInboundError protocolError)
          throwIO protocolError
        now <- getMonotonicTime
        (penaltyCount, peerState') <-
          applyReceivedTxs now [ (txId sig, sig) | sig <- sigs ] peerState
        peerState'' <-
          if penaltyCount == 0
             then pure peerState'
             else do
               let (score, ps) = State.applyPeerEvents policy now 0 penaltyCount peerState'
               traceWith tracer $
                 TraceTxSubmissionProcessed ProcessedTxCount {
                     ptxcAccepted = 0,
                     ptxcRejected = penaltyCount,
                     ptxcScore    = score
                   }
               pure ps
        -- sigs that were requested but not received count as rejected for the
        -- announciness metric (mirrors the old @CollectSigs@ handling).
        let notReceived = Map.keysSet requested Set.\\ Map.keysSet received
        metricState' <- atomically $
          foldM (\st sigid -> reportSig st (Identity (sigid, SigRejected)))
                metricState (Set.toList notReceived)
        continueAfterReplies n peerState'' metricState'

    -- Sigs whose received size does not match the advertised size (within a
    -- fuzzy tolerance).
    collectWrongSizedSigs :: Map.Map sigid SizeInBytes
                          -> Map.Map sigid sig
                          -> [(sigid, SizeInBytes, SizeInBytes)]
    collectWrongSizedSigs requested received =
      [ (sigid', receivedSize, advertisedSize)
      | (sigid', sig) <- Map.toList received
      , let receivedSize = sigSize sig
      , Just advertisedSize <- [Map.lookup sigid' requested]
      , not (checkSigSize receivedSize advertisedSize)
      ]

    checkSigSize :: SizeInBytes -> SizeInBytes -> Bool
    checkSigSize received advertised
      | received > advertised = received - advertised <= const_MAX_TX_SIZE_DISCREPANCY
      | otherwise             = advertised - received <= const_MAX_TX_SIZE_DISCREPANCY
