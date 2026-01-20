{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.SigSubmission.Inbound
  ( -- * SigSubmision Inbound client
    sigSubmissionInboundV2
    -- * Supporting types and APIs
  , module Submission
  , SigDecisionPolicy (..)
  , defaultSigDecisionPolicy
  ) where

import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadAsync (MonadAsync (..))
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Network.TypedProtocol

import Ouroboros.Network.ControlMessage (ControlMessageSTM,
           timeoutWithControlMessage)

import Ouroboros.Network.TxSubmission.Inbound.V2.Types (
           TxSubmissionMempoolWriter (..))

import DMQ.Protocol.SigSubmissionV2.Inbound

import DMQ.SigSubmission.Inbound.Policy
import DMQ.SigSubmission.Inbound.Registry as Submission
import DMQ.SigSubmission.Inbound.State
import DMQ.SigSubmission.Inbound.Types as Submission

-- | A sig-submission inbound side (client, sic!).
--
-- The client blocks on receiving `SigDecision` from the decision logic. If
-- there are sig's to download it pipelines two requests: first for sig's second
-- for sigid's. If there are no sig's to download, it either sends a blocking or
-- non-blocking request for sigid's.
--
sigSubmissionInboundV2
  :: forall sigid sig idx m failure.
     ( MonadDelay m
     , MonadThrow m
     , MonadAsync m
     , Ord sigid
     )
  => Tracer m (TraceSigSubmissionInbound sigid sig)
  -> SigSubmissionInitDelay
  -> TxSubmissionMempoolWriter sigid sig idx m failure
  -> PeerSigAPI m sigid sig
  -> ControlMessageSTM m
  -> SigSubmissionInboundPipelined sigid sig m ()
sigSubmissionInboundV2
    tracer
    initDelay
    TxSubmissionMempoolWriter { txId }
    PeerSigAPI {
      readSigDecision,
      handleReceivedSigIds,
      handleReceivedSigs,
      submitSigToMempool
    }
    controlMessageSTM
    =
      SigSubmissionInboundPipelined $ do
        case initDelay of
          SigSubmissionInitDelay delay -> threadDelay delay
          NoSigSubmissionInitDelay     -> return ()
        inboundIdle
  where
    inboundIdle
      :: m (InboundStIdle Z sigid sig m ())
    inboundIdle = do
      -- TODO
      -- readSigDecision is blocking on next decision because takeMVar and ControlMessageSTM is blocking
      sigDecision <- async readSigDecision
      msigd <- timeoutWithControlMessage controlMessageSTM (waitSTM sigDecision)
      case msigd of
        Nothing -> pure (SendMsgDone $ return ())
        Just sigd@SigDecision
              { sigdSigsToRequest = sigsToRequest
              , sigdSigsToMempool = TxsToMempool { listOfTxsToMempool }
              } -> do
                     traceWith tracer (TraceSigInboundDecision sigd)

                     let !collected = length listOfTxsToMempool

                     -- Only attempt to add sigs if we have some work to do
                     when (collected > 0) $ do
                       -- submitTxToMempool traces:
                       -- * `TraceTxSubmissionProcessed`,
                       -- * `TraceTxInboundAddedToMempool`, and
                       -- * `TraceTxInboundRejectedFromMempool`
                       -- events.
                       mapM_ (uncurry $ submitSigToMempool tracer) listOfTxsToMempool

                     -- TODO:
                     -- We can update the state so that other `sig-submission` servers will
                     -- not try to add these sigs to the mempool.
                     if Map.null sigsToRequest
                       then serverReqSigIds Zero sigd
                       else serverReqSigs sigd


    -- Pipelined request of sigs
    serverReqSigs :: SigDecision sigid sig
                 -> m (InboundStIdle Z sigid sig m ())
    serverReqSigs sigd@SigDecision { sigdSigsToRequest = sigdSigsToRequest } =
      pure $ SendMsgRequestSigsPipelined sigdSigsToRequest
                                         (serverReqSigIds (Succ Zero) sigd)

    serverReqSigIds :: forall (n :: N).
                      Nat n
                   -> SigDecision sigid sig
                   -> m (InboundStIdle n sigid sig m ())
    serverReqSigIds
      n SigDecision { sigdSigIdsToRequest = 0 }
      =
      case n of
        Zero   -> inboundIdle
        Succ _ -> handleReplies n

    serverReqSigIds
      -- if there are no unacknowledged sigids, the protocol requires sending
      -- a blocking `MsgRequestSigIds` request.  This is important, as otherwise
      -- the client side wouldn't have a chance to terminate the
      -- mini-protocol.
      Zero SigDecision { sigdSigIdsToAcknowledge = sigIdsToAck,
                         sigdPipelineSigIds      = False,
                         sigdSigIdsToRequest     = sigIdsToReq
                       }
      =
      pure $ SendMsgRequestSigIdsBlocking
                sigIdsToAck sigIdsToReq
                (\sigids -> do
                   let sigidsSeq = StrictSeq.fromList $ fst <$> sigids
                       sigidsMap = Map.fromList sigids
                   unless (StrictSeq.length sigidsSeq <= fromIntegral sigIdsToReq) $
                     throwIO ProtocolErrorTxIdsNotRequested
                   handleReceivedSigIds sigIdsToReq sigidsSeq sigidsMap
                   inboundIdle
                )

    serverReqSigIds
      n@Zero SigDecision { sigdSigIdsToAcknowledge = sigIdsToAck,
                           sigdPipelineSigIds      = True,
                           sigdSigIdsToRequest     = sigIdsToReq
                         }
      =
      pure $ SendMsgRequestSigIdsPipelined
                sigIdsToAck sigIdsToReq
                (handleReplies (Succ n))

    serverReqSigIds
      n@Succ{} SigDecision { sigdSigIdsToAcknowledge = sigIdsToAck,
                             sigdPipelineSigIds,
                             sigdSigIdsToRequest     = sigIdsToReq
                           }
      =
      -- it is impossible that we have had `sig`'s to request (Succ{} - is an
      -- evidence for that), but no unacknowledged `sigid`s.
      assert sigdPipelineSigIds $
      pure $ SendMsgRequestSigIdsPipelined
               sigIdsToAck sigIdsToReq
               (handleReplies (Succ n))


    handleReplies :: forall (n :: N).
                   Nat (S n)
                -> m (InboundStIdle (S n) sigid sig m ())
    handleReplies (Succ n'@Succ{}) =
      pure $ CollectPipelined
                Nothing
                (handleReply (handleReplies n'))

    handleReplies (Succ Zero) =
      pure $ CollectPipelined
                Nothing
                (handleReply inboundIdle)

    handleReply :: forall (n :: N).
                   m (InboundStIdle n sigid sig m ())
                   -- continuation
                -> Collect sigid sig
                -> m (InboundStIdle n sigid sig m ())
    handleReply k = \case
      CollectSigIds sigIdsToReq sigids -> do
        let sigidsSeq = StrictSeq.fromList $ fst <$> sigids
            sigidsMap = Map.fromList sigids
        unless (StrictSeq.length sigidsSeq <= fromIntegral sigIdsToReq) $
          throwIO ProtocolErrorTxIdsNotRequested
        handleReceivedSigIds sigIdsToReq sigidsSeq sigidsMap
        k
      CollectSigs sigids sigs -> do
        let requested = Map.keysSet sigids
            received  = Map.fromList [ (txId sig, sig) | sig <- sigs ]

        unless (Map.keysSet received `Set.isSubsetOf` requested) $
          throwIO ProtocolErrorTxNotRequested

        mbe <- handleReceivedSigs sigids received
        traceWith tracer $ TraceSigSubmissionCollected (txId `map` sigs)
        case mbe of
          -- one of `sig`s had a wrong size
          Just e  -> traceWith tracer (TraceSigInboundError e)
                  >> throwIO e
          Nothing -> k
