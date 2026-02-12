{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.Protocol.SigSubmissionV2.Direct (directPipelined) where

import Data.Map.Strict qualified as Map
import Data.List.NonEmpty qualified as NonEmpty

import Network.TypedProtocol.Core
import Network.TypedProtocol.Proofs (Queue (..), enqueue)

import DMQ.Protocol.SigSubmissionV2.Inbound
import DMQ.Protocol.SigSubmissionV2.Outbound
import DMQ.Protocol.SigSubmissionV2.Type (BlockingReplyList (..),
           SingBlockingStyle (..))


directPipelined
  :: forall sigId sig m a.
     Monad m
  => SigSubmissionOutbound         sigId sig m a
  -> SigSubmissionInboundPipelined sigId sig m a
  -> m a
directPipelined (SigSubmissionOutbound mOutbound)
                (SigSubmissionInboundPipelined mInbound) = do
    outbound <- mOutbound
    inbound <- mInbound
    directSender EmptyQ inbound outbound
  where
    directSender :: forall (n :: N).
                    Queue          n (Collect sigId sig)
                 -> InboundStIdle n sigId sig m a
                 -> OutboundStIdle    sigId sig m a
                 -> m a
    directSender q (SendMsgRequestSigIdsBlocking ackNo reqNo inboundNext)
                   OutboundStIdle{recvMsgRequestSigIds} = do
      reply <- recvMsgRequestSigIds SingBlocking ackNo reqNo
      case reply of
        SendMsgReplySigIds (BlockingReply sigIds) outbound' -> do
          inbound' <- inboundNext (NonEmpty.toList sigIds)
          directSender q inbound' outbound'

        SendMsgReplyNoSigIds outbound' -> do
          inbound' <- inboundNext []
          directSender q inbound' outbound'

    directSender q (SendMsgRequestSigIdsPipelined ackNo reqNo inboundNext)
                   OutboundStIdle{recvMsgRequestSigIds} = do
      reply <- recvMsgRequestSigIds SingNonBlocking ackNo reqNo
      case reply of
        SendMsgReplySigIds (NonBlockingReply sigIds) outbound' -> do
          inbound' <- inboundNext
          directSender (enqueue (CollectSigIds reqNo sigIds) q) inbound' outbound'

    directSender q (SendMsgRequestSigsPipelined sigIds inboundNext)
                   OutboundStIdle{recvMsgRequestSigs} = do
      SendMsgReplySigs sigs outbound' <- recvMsgRequestSigs $ Map.keys sigIds
      inbound' <- inboundNext
      directSender (enqueue (CollectSigs sigIds sigs) q) inbound' outbound'

    directSender q (CollectPipelined (Just noWaitInbound') _inboundNext) outbound = do
      directSender q noWaitInbound' outbound

    directSender (ConsQ c q) (CollectPipelined _maybeNoWaitInbound' inboundNext) outbound = do
      inbound' <- inboundNext c
      directSender q inbound' outbound

    directSender EmptyQ (SendMsgDone v) _outbound = v
