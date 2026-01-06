{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A view of the sig diffusion protocol from the point of view of
-- the outbound/server peer.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, 'sigSubmissionOutboundPeer' is provided for conversion
-- into the typed protocol.
module DMQ.Protocol.SigSubmissionV2.Outbound
  ( -- * Protocol type for the outbound
    SigSubmissionOutbound (..)
  , OutboundStIdle (..)
  , OutboundStSigIds (..)
  , OutboundStSigs (..)
    -- * Execution as a typed protocol
  , sigSubmissionV2OutboundPeer
  ) where

import Data.Functor ((<&>))
import Data.Singletons (SingI)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer (Peer)
import Network.TypedProtocol.Peer.Server

import DMQ.Protocol.SigSubmissionV2.Type

-- | The outbound side of the sig diffusion protocol.
--
-- The peer in the outbound/server role submits sigs to the peer in the
-- inbound/client role.
newtype SigSubmissionOutbound sigId sig m a = SigSubmissionOutbound {
      runSigSubmissionOutbound :: m (OutboundStIdle sigId sig m a)
    }

-- | In the 'StIdle' protocol state, the outbound does not have agency. Instead
-- it is waiting for:
--
-- * a request for sig ids (blocking or non-blocking)
-- * a request for a given list of sigs
-- * a termination message
--
-- It must be prepared to handle any of these.
data OutboundStIdle sigId sig m a = OutboundStIdle {
      recvMsgRequestSigIds :: forall blocking.
                              SingBlockingStyle blocking
                           -> NumIdsAck
                           -> NumIdsReq
                           -> m (OutboundStSigIds blocking sigId sig m a),

      recvMsgRequestSigs   :: [sigId]
                           -> m (OutboundStSigs sigId sig m a),

      recvMsgDone          :: m a
    }

data OutboundStSigIds blocking sigId sig m a where
  SendMsgReplySigIds
    :: SingI blocking
    => BlockingReplyList blocking sigId
    -> OutboundStIdle sigId sig m a
    -> OutboundStSigIds blocking sigId sig m a

  SendMsgReplyNoSigIds
    :: OutboundStIdle sigId sig m a
    -> OutboundStSigIds StBlocking sigId sig m a

data OutboundStSigs sigId sig m a where
  SendMsgReplySigs
    :: [sig]
    -> OutboundStIdle sigId sig m a
    -> OutboundStSigs sigId sig m a


-- | A non-pipelined 'Peer' representing the 'SigSubmissionOutbound'.
sigSubmissionV2OutboundPeer
  :: forall sigId sig m a.
     Monad m
  => SigSubmissionOutbound sigId sig m a
  -> Peer (SigSubmissionV2 sigId sig) AsServer NonPipelined StIdle m a
sigSubmissionV2OutboundPeer (SigSubmissionOutbound outboundSt) =
    Effect (run <$> outboundSt)
  where
    run :: OutboundStIdle sigId sig m a
        -> Peer (SigSubmissionV2 sigId sig) AsServer NonPipelined StIdle m a
    run OutboundStIdle {recvMsgRequestSigIds, recvMsgRequestSigs, recvMsgDone} =
      Await $ \case
        MsgRequestSigIds blocking ackNo reqNo -> Effect $ do
          recvMsgRequestSigIds blocking ackNo reqNo <&> \case

            SendMsgReplySigIds sigIds k ->
              Yield
                (MsgReplySigIds sigIds)
                (run k)

            SendMsgReplyNoSigIds k ->
              Yield
                MsgReplyNoSigIds
                (run k)

        MsgRequestSigs sigIds -> Effect $ do
          recvMsgRequestSigs sigIds <&> \case
            SendMsgReplySigs sigs k ->
              Yield
                (MsgReplySigs sigs)
                (run k)

        MsgDone -> Effect $ Done <$> recvMsgDone
