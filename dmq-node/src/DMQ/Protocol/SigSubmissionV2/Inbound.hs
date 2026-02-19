{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A view of the sig submission protocol from the point of view of the
-- inbound/client peer.
--
-- This provides a view that uses less complex types and should be easier to use
-- than the underlying typed protocol itself.
--
-- For execution, a conversion into the typed protocol is provided.
module DMQ.Protocol.SigSubmissionV2.Inbound
  ( -- * Protocol type for the inbound
    SigSubmissionInboundPipelined (..)
  , InboundStIdle (..)
  , Collect (..)
    -- * Execution as a typed protocol
  , sigSubmissionV2InboundPeerPipelined
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer (Peer, PeerPipelined (..))
import Network.TypedProtocol.Peer.Client
import DMQ.Protocol.SigSubmissionV2.Type

data SigSubmissionInboundPipelined sigId sig m a where
  SigSubmissionInboundPipelined
    :: m (InboundStIdle Z sigId sig m a)
    -> SigSubmissionInboundPipelined sigId sig m a

-- | This is the type of the pipelined results, collected by 'CollectPipelined'.
-- This protocol can pipeline requests for identifiers and signatures, so we use
-- a sum of either for collecting the responses.
--
data Collect sigId sig
  = -- | The result of 'SendMsgRequestSigIdsPipelined'. It also carries
    -- the number of sigIds originally requested.
    CollectSigIds NumIdsReq [(sigId, SizeInBytes)]

  | -- | The result of 'SendMsgRequestSigsPipelined'. The actual reply only
    -- contains the signatures sent, but this pairs them up with the
    -- requested identifiers. This is for the peer to determine whether some
    -- signatures are no longer needed.
    CollectSigs (Map sigId SizeInBytes) [sig]


data InboundStIdle (n :: N) sigId sig m a where
  SendMsgRequestSigIdsBlocking
    :: NumIdsAck -- ^ number of sigIds to acknowledge
    -> NumIdsReq -- ^ number of sigIds to request
    -> ([(sigId, SizeInBytes)] -> m (InboundStIdle Z sigId sig m a))
    -> InboundStIdle Z sigId sig m a

  SendMsgRequestSigIdsPipelined
    :: NumIdsAck
    -> NumIdsReq
    -> m (InboundStIdle (S n) sigId sig m a)
    -> InboundStIdle n sigId sig m a

  SendMsgRequestSigsPipelined
    :: Map sigId SizeInBytes
    -> m (InboundStIdle (S n) sigId sig m a)
    -> InboundStIdle n sigId sig m a

  CollectPipelined
    :: Maybe (InboundStIdle (S n) sigId sig m a)
    -> (Collect sigId sig -> m (InboundStIdle n sigId sig m a))
    -> InboundStIdle (S n) sigId sig m a

  SendMsgDone
    :: m a
    -> InboundStIdle Z sigId sig m a


-- | Transform a 'SigSubmissionInboundPipelined' into a 'PeerPipelined'.
--
sigSubmissionV2InboundPeerPipelined
  :: forall sigId sig m a.
     (Functor m)
  => SigSubmissionInboundPipelined sigId sig m a
  -> PeerPipelined (SigSubmissionV2 sigId sig) AsClient StIdle m a
sigSubmissionV2InboundPeerPipelined (SigSubmissionInboundPipelined inboundSt) =
  PeerPipelined $ Effect (run <$> inboundSt)
  where
    run :: InboundStIdle n sigId sig m a
        -> Peer (SigSubmissionV2 sigId sig) AsClient (Pipelined n (Collect sigId sig)) StIdle m a

    run (SendMsgRequestSigIdsBlocking ackNo reqNo k) =
      Yield (MsgRequestSigIds SingBlocking ackNo reqNo) $
        Await \case
          MsgReplySigIds (BlockingReply sigIds) ->
            Effect $ run <$> k (NonEmpty.toList sigIds)

          MsgReplyNoSigIds ->
            Effect $ run <$> k []

    run (SendMsgRequestSigIdsPipelined ackNo reqNo k) =
      YieldPipelined
        (MsgRequestSigIds SingNonBlocking ackNo reqNo)
        (ReceiverAwait
          $ \(MsgReplySigIds (NonBlockingReply sigIds)) ->
              ReceiverDone (CollectSigIds reqNo sigIds)
        )
        (Effect $ run <$> k)

    run (SendMsgRequestSigsPipelined sigIds k) =
      YieldPipelined
        (MsgRequestSigs $ Map.keys sigIds)
        (ReceiverAwait
          $ \(MsgReplySigs sigs) ->
              ReceiverDone (CollectSigs sigIds sigs)
        )
        (Effect $ run <$> k)

    run (CollectPipelined none collect) =
      Collect
        (run <$> none)
        (Effect . fmap run . collect)

    run (SendMsgDone done) =
      Effect $ Yield MsgDone . Done <$> done
