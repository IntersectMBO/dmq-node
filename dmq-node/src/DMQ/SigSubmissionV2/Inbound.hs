{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.SigSubmissionV2.Inbound
  ( -- * SigSubmision Inbound client
    sigSubmissionInbound
  ) where

import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadAsync (MonadAsync (..))
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, traceWith)

import Network.TypedProtocol

import Ouroboros.Network.ControlMessage (ControlMessageSTM,
           timeoutWithControlMessage)
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToReq(..),
           NumTxIdsToAck (..))
import Ouroboros.Network.TxSubmission.Inbound.V2 (TraceTxSubmissionInbound (..),
           PeerTxAPI (..), TxDecision (..), TxsToMempool (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.Types (
           TxSubmissionMempoolWriter (..))

import DMQ.Protocol.SigSubmissionV2.Inbound
import DMQ.Protocol.SigSubmissionV2.Type (NumIdsReq(..), NumIdsAck (NumIdsAck))
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
     , Ord sigid
     )
  => Tracer m (TraceTxSubmissionInbound sigid sig)
  -> TxSubmissionMempoolWriter sigid sig idx m failure
  -> PeerTxAPI m sigid sig
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
    controlMessageSTM
    =
      SigSubmissionInboundPipelined inboundIdle
  where
    inboundIdle
      :: m (InboundStIdle Z sigid sig m ())
    inboundIdle = do
      -- TODO
      -- readSigDecision is blocking on next decision because takeMVar and ControlMessageSTM is blocking
      sigDecision <- async readTxDecision
      msigd <- timeoutWithControlMessage controlMessageSTM (waitSTM sigDecision)
      case msigd of
        Nothing -> pure (SendMsgDone $ return ())
        Just sigd@TxDecision
              { txdTxsToRequest = sigsToRequest
              , txdTxsToMempool = TxsToMempool { listOfTxsToMempool }
              } -> do
                     traceWith tracer (TraceTxInboundDecision sigd)

                     let !collected = length listOfTxsToMempool

                     -- Only attempt to add sigs if we have some work to do
                     when (collected > 0) $ do
                       -- submitTxToMempool traces:
                       -- * `TraceTxSubmissionProcessed`,
                       -- * `TraceTxInboundAddedToMempool`, and
                       -- * `TraceTxInboundRejectedFromMempool`
                       -- events.
                       mapM_ (uncurry $ submitTxToMempool tracer) listOfTxsToMempool

                     -- TODO:
                     -- We can update the state so that other `sig-submission` servers will
                     -- not try to add these sigs to the mempool.
                     if Map.null sigsToRequest
                       then clientReqSigIds Zero sigd
                       else clientReqSigs sigd


    -- Pipelined request of sigs
    clientReqSigs :: TxDecision sigid sig
                 -> m (InboundStIdle Z sigid sig m ())
    clientReqSigs sigd@TxDecision { txdTxsToRequest = sigdSigsToRequest } =
      pure $ SendMsgRequestSigsPipelined sigdSigsToRequest
                                         (clientReqSigIds (Succ Zero) sigd)

    clientReqSigIds :: forall (n :: N).
                      Nat n
                   -> TxDecision sigid sig
                   -> m (InboundStIdle n sigid sig m ())
    clientReqSigIds
      n TxDecision { txdTxIdsToRequest = 0 }
      =
      case n of
        Zero   -> inboundIdle
        Succ _ -> handleReplies n

    clientReqSigIds
      -- if there are no unacknowledged sigids, the protocol requires sending
      -- a blocking `MsgRequestSigIds` request.  This is important, as otherwise
      -- the client side wouldn't have a chance to terminate the
      -- mini-protocol.
      Zero TxDecision { txdTxIdsToAcknowledge = sigIdsToAck,
                        txdPipelineTxIds      = False,
                        txdTxIdsToRequest     = sigIdsToReq
                      }
      =
      pure $ SendMsgRequestSigIdsBlocking
                (NumIdsAck . getNumTxIdsToAck $ sigIdsToAck)
                (NumIdsReq . getNumTxIdsToReq $ sigIdsToReq)
                (\sigids -> do
                   let sigidsSeq = StrictSeq.fromList $ fst <$> sigids
                       sigidsMap = Map.fromList sigids
                   unless (StrictSeq.length sigidsSeq <= fromIntegral sigIdsToReq) $
                     throwIO ProtocolErrorSigIdsNotRequested
                   handleReceivedTxIds sigIdsToReq sigidsSeq sigidsMap
                   inboundIdle
                )

    clientReqSigIds
      n@Zero TxDecision { txdTxIdsToAcknowledge = sigIdsToAck,
                          txdPipelineTxIds      = True,
                          txdTxIdsToRequest     = sigIdsToReq
                        }
      =
      pure $ SendMsgRequestSigIdsPipelined
                (NumIdsAck . getNumTxIdsToAck $ sigIdsToAck)
                (NumIdsReq . getNumTxIdsToReq $ sigIdsToReq)
                (handleReplies (Succ n))

    clientReqSigIds
      n@Succ{} TxDecision { txdTxIdsToAcknowledge = sigIdsToAck,
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
          throwIO ProtocolErrorSigIdsNotRequested
        handleReceivedTxIds (NumTxIdsToReq . getNumIdsReq $ sigIdsToReq) sigidsSeq sigidsMap
        k
      CollectSigs sigids sigs -> do
        let requested = Map.keysSet sigids
            received  = Map.fromList [ (txId sig, sig) | sig <- sigs ]

        unless (Map.keysSet received `Set.isSubsetOf` requested) $
          throwIO ProtocolErrorSigNotRequested

        mbe <- handleReceivedTxs sigids received
        traceWith tracer $ TraceTxSubmissionCollected (txId `map` sigs)
        case mbe of
          -- one of `sig`s had a wrong size
          Just e  -> traceWith tracer (TraceTxInboundError e)
                  >> throwIO e
          Nothing -> k
