{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.SigSubmissionV2.Outbound
  ( sigSubmissionOutbound
  , TraceSigSubmissionOutbound (..)
  , SigSubmissionProtocolError (..)
  ) where

import Data.Aeson (ToJSON (toJSON), Value (String), object, KeyValue ((.=)))
import Data.Foldable (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as Seq
import Data.Word (Word16)

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer (..), traceWith)

import Ouroboros.Network.TxSubmission.Mempool.Reader (MempoolSnapshot (..),
           TxSubmissionMempoolReader (..))
 
import DMQ.Protocol.SigSubmissionV2.Outbound
import DMQ.Protocol.SigSubmissionV2.Type


data TraceSigSubmissionOutbound sigId sig
  = TraceSigSubmissionOutboundRecvMsgRequestSigs
      [sigId]
      -- ^ The IDs of the signatures requested.
  | TraceSigSubmissionOutboundSendMsgReplySigs
      [sig]
      -- ^ The sigs to be sent in the response.
  deriving Show

instance (ToJSON sigId, ToJSON sig)
      => ToJSON (TraceSigSubmissionOutbound sigId sig) where
  toJSON (TraceSigSubmissionOutboundRecvMsgRequestSigs sigIds) =
    object
      [ "kind"  .= String "SigSubmissionOutboundRecvMsgRequestSigs"
      , "sigIds" .= sigIds
      ]
  toJSON (TraceSigSubmissionOutboundSendMsgReplySigs sigs) =
    object
      [ "kind" .= String "SigSubmissionOutboundSendMsgReplySigs"
      , "sigs"  .= sigs
      ]

data SigSubmissionProtocolError =
       ProtocolErrorAckedTooManySigIds
     | ProtocolErrorRequestedNothing
     | ProtocolErrorRequestedTooManySigIds NumIdsReq Word16 NumIdsAck
     | ProtocolErrorRequestBlocking
     | ProtocolErrorRequestNonBlocking
     | ProtocolErrorRequestedUnavailableSig
  deriving Show

instance Exception SigSubmissionProtocolError where
  displayException ProtocolErrorAckedTooManySigIds =
      "The peer tried to acknowledged more sigIds than are available to do so."

  displayException (ProtocolErrorRequestedTooManySigIds reqNo unackedNo maxUnacked) =
      "The peer requested " ++ show reqNo ++ " sigIds which would put the "
   ++ "total in flight over the limit of " ++ show maxUnacked ++ "."
   ++ " Number of unacked sigIds " ++ show unackedNo

  displayException ProtocolErrorRequestedNothing =
      "The peer requested zero sigIds."

  displayException ProtocolErrorRequestBlocking =
      "The peer made a blocking request for more sigIds when there are still "
   ++ "unacknowledged sigIds. It should have used a non-blocking request."

  displayException ProtocolErrorRequestNonBlocking =
      "The peer made a non-blocking request for more sigIds when there are "
   ++ "no unacknowledged sigIds. It should have used a blocking request."

  displayException ProtocolErrorRequestedUnavailableSig =
      "The peer requested a signature which is not available, either "
   ++ "because it was never available or because it was previously requested."


sigSubmissionOutbound
  :: forall version sigId sig idx m.
     (Ord sigId, Ord idx, MonadSTM m, MonadThrow m)
  => Tracer m (TraceSigSubmissionOutbound sigId sig)
  -> NumIdsAck  -- ^ Maximum number of unacknowledged sigIds allowed
  -> TxSubmissionMempoolReader sigId sig idx m
  -> version
  -> SigSubmissionOutbound sigId sig m ()
sigSubmissionOutbound tracer maxUnacked TxSubmissionMempoolReader{..} _version =
    SigSubmissionOutbound (pure (server Seq.empty mempoolZeroIdx))
  where
    server :: StrictSeq (sigId, idx) -> idx -> OutboundStIdle sigId sig m ()
    server !unackedSeq !lastIdx =
        OutboundStIdle { recvMsgRequestSigIds, recvMsgRequestSigs, recvMsgDone }
      where
        recvMsgDone :: m ()
        recvMsgDone = pure ()
  
        recvMsgRequestSigIds :: forall blocking.
                               SingBlockingStyle blocking
                            -> NumIdsAck
                            -> NumIdsReq
                            -> m (OutboundStSigIds blocking sigId sig m ())
        recvMsgRequestSigIds blocking ackNo reqNo = do
          when (getNumIdsAck ackNo > fromIntegral (Seq.length unackedSeq)) $
            throwIO ProtocolErrorAckedTooManySigIds

          let unackedNo = fromIntegral (Seq.length unackedSeq)
          when (  unackedNo
                - getNumIdsAck ackNo
                + getNumIdsReq reqNo
                > getNumIdsAck maxUnacked) $
            throwIO (ProtocolErrorRequestedTooManySigIds reqNo unackedNo maxUnacked)

          -- Update our tracking state to remove the number of sigIds that the
          -- peer has acknowledged.
          let !unackedSeq' = Seq.drop (fromIntegral ackNo) unackedSeq

          -- Update our tracking state with any extra sigs available.
          let update sigs =
                -- These sigs should all be fresh
                assert (all (\(_, idx, _) -> idx > lastIdx) sigs) $
                  let !unackedSeq'' = unackedSeq' <> Seq.fromList
                                        [ (sigId, idx) | (sigId, idx, _) <- sigs ]
                      !lastIdx'
                        | null sigs = lastIdx
                        | otherwise = idx where (_, idx, _) = last sigs
                      sigs'        :: [(sigId, SizeInBytes)]
                      sigs'         = [ (sigId, size) | (sigId, _, size) <- sigs ]
                      server'       = server unackedSeq'' lastIdx'
                  in  (sigs', server')

          -- Grab info about any new sigs after the last sig idx we've seen,
          -- up to the number that the peer has requested.
          case blocking of
            SingBlocking -> do
              when (reqNo == 0) $
                throwIO ProtocolErrorRequestedNothing
              unless (Seq.null unackedSeq') $
                throwIO ProtocolErrorRequestBlocking

              sigs <- atomically $
                do
                  MempoolSnapshot{mempoolTxIdsAfter} <- mempoolGetSnapshot
                  let sigs = mempoolTxIdsAfter lastIdx
                  check (not $ null sigs)
                  pure (take (fromIntegral reqNo) sigs)

              let !(sigs', server') = update sigs
                  sigs'' = case NonEmpty.nonEmpty sigs' of
                    Just x -> x
                    -- Assert sigs is non-empty: we blocked until sigs was non-null,
                    -- and we know reqNo > 0, hence `take reqNo sigs` is non-null.
                    Nothing -> error "sigSubmissionOutbound: empty signature list"
              pure (SendMsgReplySigIds (BlockingReply sigs'') server')

            SingNonBlocking -> do
              when (reqNo == 0 && ackNo == 0) $
                throwIO ProtocolErrorRequestedNothing
              when (Seq.null unackedSeq') $
                throwIO ProtocolErrorRequestNonBlocking

              sigs <- atomically $ do
                MempoolSnapshot{mempoolTxIdsAfter} <- mempoolGetSnapshot
                let sigs = mempoolTxIdsAfter lastIdx
                return (take (fromIntegral reqNo) sigs)

              let !(sigs', server') = update sigs
              pure (SendMsgReplySigIds (NonBlockingReply sigs') server')

        recvMsgRequestSigs :: [sigId]
                          -> m (OutboundStSigs sigId sig m ())
        recvMsgRequestSigs sigIds = do
          -- Trace the IDs of the signatures requested.
          traceWith tracer (TraceSigSubmissionOutboundRecvMsgRequestSigs sigIds)

          MempoolSnapshot{mempoolLookupTx} <- atomically mempoolGetSnapshot

          -- The window size is expected to be small (currently 10) so the find is acceptable.
          let sigIdxs  = [ find (\(t,_) -> t == sigId) unackedSeq | sigId <- sigIds ]
              sigIdxs' = map snd $ catMaybes sigIdxs

          when (any isNothing sigIdxs) $
            throwIO ProtocolErrorRequestedUnavailableSig

          -- The 'mempoolLookupTx' will return nothing if the signature is no
          -- longer in the mempool. This is good. Neither the sending nor
          -- receiving side wants to forward sigs that are no longer of interest.
          let sigs    = mapMaybe mempoolLookupTx sigIdxs'
              server' = server unackedSeq lastIdx

          -- Trace the sigs to be sent in the response.
          traceWith tracer (TraceSigSubmissionOutboundSendMsgReplySigs sigs)

          return $ SendMsgReplySigs sigs server'
