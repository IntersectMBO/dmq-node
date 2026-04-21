{-# LANGUAGE BangPatterns        #-}
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
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad (foldM, unless)
import Control.Monad.Class.MonadAsync (MonadAsync (..))
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import "contra-tracer" Control.Tracer (Tracer, traceWith)

import Network.TypedProtocol

import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..),
           NumTxIdsToReq (..))
import Ouroboros.Network.TxSubmission.Inbound.V2 (PeerTxAPI (..),
           TraceTxSubmissionInbound (..), TxDecision (..), TxsToMempool (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
           (TxSubmissionMempoolWriter (..))

import DMQ.Diffusion.PeerSelection.PeerMetric (LocalPeerMetricState,
           ReportPeerMetric' (..), ReportPeerMetricI, TxMempoolResult (..))
import DMQ.Diffusion.PeerSelection.PeerMetric qualified as PeerMetric
import DMQ.Protocol.SigSubmissionV2.Inbound
import DMQ.Protocol.SigSubmissionV2.Type (NumIdsAck (NumIdsAck), NumIdsReq (..))
import DMQ.SigSubmissionV2.Types


-- | A sig-submission inbound side (client, sic!).
--
-- The client blocks on receiving `SigDecision` from the decision logic. If
-- there are sig's to download it pipelines two requests: first for sig's second
-- for sigid's. If there are no sig's to download, it either sends a blocking or
-- non-blocking request for sigid's.
--
sigSubmissionInbound
  :: forall sigid sig idx m failure.
     ( MonadThrow m
     , MonadAsync m
     , Ord sigid, MonadMonotonicTime m
     )
  => Tracer m (TraceTxSubmissionInbound sigid sig)
  -> TxSubmissionMempoolWriter sigid sig idx m failure
  -> PeerTxAPI m sigid sig
  -> ReportPeerMetricI m sigid
  -> ControlMessageSTM m
  -> SigSubmissionInboundPipelined sigid sig m ()
sigSubmissionInbound
    tracer
    TxSubmissionMempoolWriter { txId }
    PeerTxAPI {
      readTxDecision,
      handleReceivedTxIds,
      handleReceivedTxs,
      submitTxToMempool
    }
    ReportPeerMetric {
      reportSigId,
      reportSig
    }
    controlMessageSTM
    =
    SigSubmissionInboundPipelined (inboundIdle PeerMetric.emptyLocalPeerMetricState)
  where
    inboundIdle
      :: LocalPeerMetricState sigid
      -> m (InboundStIdle Z sigid sig m ())
    inboundIdle localState = do

      -- NOTE: The `tx-logic` is using `MVar` API, while `controlMessage` an
      -- `STM`, we need to compose both.  We do that in two steps to avoid
      -- Situation where the `tx-logic` is much quicker to reply, and wins the
      -- race each time.
      k <-
        -- 1. non-blocking check of `ControlMessage`
        atomically controlMessageSTM >>= \case
          Terminate -> return (Left ())
          _ ->
            -- 2. race  `readTXDecision` and `controlMessageSTM`
            atomically (do
              cntrlMsg <- controlMessageSTM
              case cntrlMsg of
                Terminate -> return ()
                Continue  -> retry
                Quiesce   -> retry
              )
            `race`
            readTxDecision

      case k of
        Left{} -> pure (SendMsgDone $ return ())
        Right sigd@TxDecision
              { txdTxsToRequest = sigsToRequest
              , txdTxsToMempool = TxsToMempool { listOfTxsToMempool }
              } -> do
          traceWith tracer (TraceTxInboundDecision sigd)

          let !collected = length listOfTxsToMempool

          -- Only attempt to add sigs if we have some work to do
          localState' <-
            if collected > 0
            then do
              -- submitTxToMempool traces:
              -- * `TraceTxSubmissionProcessed`,
              -- * `TraceTxInboundAddedToMempool`, and
              -- * `TraceTxInboundRejectedFromMempool`
              -- events.
              foldM (\st (sigid, sig) -> do
                      r <- submitTxToMempool tracer sigid sig
                      atomically $ reportSig st (Identity (sigid, r))
                    )
                    localState
                    listOfTxsToMempool
          else return localState

          -- TODO:
          -- We can update the state so that other `sig-submission` servers will
          -- not try to add these sigs to the mempool.
          if Map.null sigsToRequest
            then clientReqSigIds Zero localState' sigd
            else clientReqSigs localState' sigd


    -- Pipelined request of sigs
    clientReqSigs :: LocalPeerMetricState sigid
                  -> TxDecision sigid sig
                 -> m (InboundStIdle Z sigid sig m ())
    clientReqSigs localState sigd@TxDecision { txdTxsToRequest = sigdSigsToRequest } =
      pure $ SendMsgRequestSigsPipelined sigdSigsToRequest
                                         (clientReqSigIds (Succ Zero) localState sigd)

    clientReqSigIds :: forall (n :: N).
                      Nat n
                   -> LocalPeerMetricState sigid
                   -> TxDecision sigid sig
                   -> m (InboundStIdle n sigid sig m ())
    clientReqSigIds
      n localState TxDecision { txdTxIdsToRequest = 0 }
      =
      case n of
        Zero   -> inboundIdle localState
        Succ _ -> handleReplies n localState

    clientReqSigIds
      -- if there are no unacknowledged sigids, the protocol requires sending
      -- a blocking `MsgRequestSigIds` request.  This is important, as otherwise
      -- the client side wouldn't have a chance to terminate the
      -- mini-protocol.
      Zero
      localState
      TxDecision { txdTxIdsToAcknowledge = sigIdsToAck,
                   txdPipelineTxIds      = False,
                   txdTxIdsToRequest     = sigIdsToReq
                 }
      =
      pure $ SendMsgRequestSigIdsBlocking
                (NumIdsAck . getNumTxIdsToAck $ sigIdsToAck)
                (NumIdsReq . getNumTxIdsToReq $ sigIdsToReq)
                (\sigids -> do
                   time <- getMonotonicTime
                   let sigidsSeq = StrictSeq.fromList $ fst <$> sigids
                       sigidsMap = Map.fromList sigids
                   unless (StrictSeq.length sigidsSeq <= fromIntegral sigIdsToReq) $
                     throwIO ProtocolErrorSigIdsNotRequested
                   let localState' =
                         foldl' (\st (sigid, _) -> reportSigId sigid time st)
                                localState
                                sigids
                   handleReceivedTxIds sigIdsToReq sigidsSeq sigidsMap
                   inboundIdle localState'
                )

    clientReqSigIds
      n@Zero
      localState
      TxDecision { txdTxIdsToAcknowledge = sigIdsToAck,
                   txdPipelineTxIds      = True,
                   txdTxIdsToRequest     = sigIdsToReq
                 }
      =
      pure $ SendMsgRequestSigIdsPipelined
                (NumIdsAck . getNumTxIdsToAck $ sigIdsToAck)
                (NumIdsReq . getNumTxIdsToReq $ sigIdsToReq)
                (handleReplies (Succ n) localState)

    clientReqSigIds
      n@Succ{}
      localState
      TxDecision { txdTxIdsToAcknowledge = sigIdsToAck,
                   txdPipelineTxIds,
                   txdTxIdsToRequest     = sigIdsToReq
                 }
      =
      -- it is impossible that we have had `sig`'s to request (Succ{} - is an
      -- evidence for that), but no unacknowledged `sigid`s.
      assert txdPipelineTxIds $
      pure $ SendMsgRequestSigIdsPipelined
               (NumIdsAck . getNumTxIdsToAck $ sigIdsToAck)
               (NumIdsReq . getNumTxIdsToReq $ sigIdsToReq)
               (handleReplies (Succ n) localState)


    handleReplies :: forall (n :: N).
                     Nat (S n)
                  -> LocalPeerMetricState sigid
                  -> m (InboundStIdle (S n) sigid sig m ())
    handleReplies (Succ n'@Succ{}) localState =
      pure $ CollectPipelined
                Nothing
                (handleReply localState (handleReplies n'))

    handleReplies (Succ Zero) localState =
      pure $ CollectPipelined
                Nothing
                (handleReply localState inboundIdle)

    handleReply :: forall (n :: N).
                   LocalPeerMetricState sigid
                -> (LocalPeerMetricState sigid -> m (InboundStIdle n sigid sig m ()))
                   -- continuation
                -> Collect sigid sig
                -> m (InboundStIdle n sigid sig m ())
    handleReply localState k = \case
      CollectSigIds sigIdsToReq sigids -> do
        time <- getMonotonicTime
        let sigidsSeq = StrictSeq.fromList $ fst <$> sigids
            sigidsMap = Map.fromList sigids
        unless (StrictSeq.length sigidsSeq <= fromIntegral sigIdsToReq) $
          throwIO ProtocolErrorSigIdsNotRequested
        let localState' =
              foldl' (\st (sigid, _) -> reportSigId sigid time st)
                     localState
                     sigids
        handleReceivedTxIds (NumTxIdsToReq . getNumIdsReq $ sigIdsToReq) sigidsSeq sigidsMap
        k localState'

      CollectSigs sigids sigs -> do
        let requested   = Map.keysSet sigids
            received    = Map.fromList [ (txId sig, sig) | sig <- sigs ]
            notReceived = requested Set.\\ Map.keysSet received

        unless (Map.keysSet received `Set.isSubsetOf` requested) $
          throwIO ProtocolErrorSigNotRequested

        localState' <- atomically do
          foldM (\st sigid -> reportSig st (Identity (sigid, TxRejected)))
                localState
                (Set.toList notReceived)

        mbe <- handleReceivedTxs sigids received
        traceWith tracer $ TraceTxSubmissionCollected (txId `map` sigs)
        case mbe of
          -- one of `sig`s had a wrong size
          Just e  -> traceWith tracer (TraceTxInboundError e)
                  >> throwIO e
          Nothing -> k localState'
