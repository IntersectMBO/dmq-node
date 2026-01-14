{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeFamilies               #-}

-- | The type of the signature diffusion protocol.
--
-- This is used to diffuse generic signatures between nodes.
--
-- It is based on `Ouroboros.Network.Protocol.ObjectDiffusion` mini-protocol
-- originally designed for Peras.
--
module DMQ.Protocol.SigSubmissionV2.Type
  ( SigSubmissionV2 (..)
  , Message (..)
  , SingSigSubmissionV2 (..)
  , NumIdsAck (..)
  , NumIdsReq (..)
  , NumReq (..)
  , NumUnacknowledged (..)
    -- Signature types
  , module SigSubmission
    -- re-exports
  , BlockingReplyList (..)
  , SingBlockingStyle (..)
  , SizeInBytes (..)
  , StBlockingStyle (..)
  ) where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (toJSON), Value (String), KeyValue ((.=)), object)
import Data.Kind (Type)
import Data.Monoid (Sum (..))
import Data.Singletons
import Data.Text (pack)
import Data.Word (Word16)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet (Quiet (..))

import Network.TypedProtocol.Codec (AnyMessage(AnyMessageAndAgency))
import Network.TypedProtocol.Core

import DMQ.Protocol.SigSubmission.Type as SigSubmission (SigId (..),
           SigBody (..), SigKESSignature (..), SigOpCertificate (..),
           SigColdKey (..), SigRaw (..), SigRawWithSignedBytes (..), Sig (..)) 
import Ouroboros.Network.Protocol.TxSubmission2.Type (BlockingReplyList (..),
           SingBlockingStyle (..), StBlockingStyle (..))
import Ouroboros.Network.SizeInBytes (SizeInBytes (..))
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

-- | The kind of the object diffusion protocol, and the types of the states in
-- the protocol state machine.
--
-- We describe this protocol using indiscriminately the labels \"inbound\"/\"client\"
-- for the peer that is receiving objects, and \"outbound\"/\"server\" for the one
-- sending them.
type SigSubmissionV2 :: Type -> Type -> Type
data SigSubmissionV2 sigId sig where
  -- | The inbound node has agency; it can either terminate, ask for object
  -- identifiers or ask for objects.
  --
  -- There is no timeout in this state.
  StIdle      :: SigSubmissionV2 sigId sig

  -- | The outbound node has agency; it must reply with a list of object
  -- identifiers that it wishes to submit.
  --
  -- There are two sub-states for this, for blocking and non-blocking cases.
  StSigIds    :: StBlockingStyle -> SigSubmissionV2 sigId sig

  -- | The outbound node has agency; it must reply with the list of
  -- objects.
  StSigs      :: SigSubmissionV2 sigId sig

  -- | Nobody has agency; termination state.
  StDone      :: SigSubmissionV2 sigId sig

instance ( ShowProxy sigId
         , ShowProxy sig
         )
      => ShowProxy (SigSubmissionV2 sigId sig) where
  showProxy _ =
    concat
      [ "SigSubmissionV2 ",
        showProxy (Proxy :: Proxy sigId),
        " ",
        showProxy (Proxy :: Proxy sig)
      ]

instance ShowProxy (StIdle :: SigSubmissionV2 sigId sig) where
  showProxy _ = "StIdle"
instance (Show sigId, Show sig)
      => ToJSON (AnyMessage (SigSubmissionV2 sigId sig)) where
  toJSON (AnyMessageAndAgency stok MsgRequestSigIds{}) =
    object
      [ "kind" .= String "MsgRequestSigIds"
      , "agency" .= String (pack $ show stok)
      ]
  toJSON (AnyMessageAndAgency stok (MsgReplySigIds ids)) =
    object
      [ "kind" .= String "MsgReplySigIds"
      , "agency" .= String (pack $ show stok)
      , "ids" .= String (pack $ show ids)
      ]
  toJSON (AnyMessageAndAgency stok MsgReplyNoSigIds) =
    object
      [ "kind" .= String "MsgReplyNoSigIds"
      , "agency" .= String (pack $ show stok)
      ]
  toJSON (AnyMessageAndAgency stok (MsgRequestSigs{})) =
    object
      [ "kind" .= String "MsgRequestSigs"
      , "agency" .= String (pack $ show stok)
      ]
  toJSON (AnyMessageAndAgency stok (MsgReplySigs sigs)) =
    object
      [ "kind" .= String "MsgReplySigs"
      , "agency" .= String (pack $ show stok)
      , "sigs" .= String (pack $ show sigs)
      ]
  toJSON (AnyMessageAndAgency stok MsgDone) =
    object
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]


type SingSigSubmissionV2
  :: SigSubmissionV2 sigId sig
  -> Type
data SingSigSubmissionV2 k where
  SingIdle   :: SingSigSubmissionV2 StIdle
  SingSigIds :: SingBlockingStyle stBlocking
             -> SingSigSubmissionV2 (StSigIds stBlocking)
  SingSigs   :: SingSigSubmissionV2 StSigs
  SingDone   :: SingSigSubmissionV2 StDone

deriving instance Show (SingSigSubmissionV2 st)

instance StateTokenI StIdle                where stateToken = SingIdle
instance SingI stBlocking
      => StateTokenI (StSigIds stBlocking) where stateToken = SingSigIds sing
instance StateTokenI StSigs                where stateToken = SingSigs
instance StateTokenI StDone                where stateToken = SingDone


newtype NumIdsAck = NumIdsAck {getNumIdsAck :: Word16}
  deriving (Eq, Ord, NFData, Generic)
  deriving newtype (Num, Enum, Real, Integral, Bounded, NoThunks)
  deriving Semigroup via (Sum Word16)
  deriving Monoid    via (Sum Word16)
  deriving Show      via (Quiet NumIdsAck)

newtype NumIdsReq = NumIdsReq {getNumIdsReq :: Word16}
  deriving (Eq, Ord, NFData, Generic)
  deriving newtype (Num, Enum, Real, Integral, Bounded, NoThunks)
  deriving Semigroup via (Sum Word16)
  deriving Monoid    via (Sum Word16)
  deriving Show      via (Quiet NumIdsReq)

newtype NumReq = NumReq {getNumReq :: Word16}
  deriving (Eq, Ord, NFData, Generic)
  deriving newtype (Num, Enum, Real, Integral, Bounded, NoThunks)
  deriving Semigroup via (Sum Word16)
  deriving Monoid    via (Sum Word16)
  deriving Show      via (Quiet NumReq)

newtype NumUnacknowledged = NumUnacknowledged {getNumUnacknowledged :: Word16}
  deriving (Eq, Ord, NFData, Generic)
  deriving newtype (Num, Enum, Real, Integral, Bounded, NoThunks)
  deriving Semigroup via (Sum Word16)
  deriving Monoid    via (Sum Word16)
  deriving Show      via (Quiet NumUnacknowledged)


-- | There are some constraints of the protocol that are not captured in the
-- types of the messages, but are documented with the messages. Violation
-- of these constraints is also a protocol error. The constraints are intended
-- to ensure that implementations are able to work in bounded space.
instance Protocol (SigSubmissionV2 sigId sig) where
  -- | The messages in the object diffusion protocol.
  --
  -- In this protocol the consumer (inbound side, client role) always
  -- initiates and the producer (outbound side, server role) replies.
  -- This makes it a pull based protocol where the receiver manages the
  -- control flow.
  --
  -- The protocol involves asking for object identifiers, and then
  -- asking for objects corresponding to the identifiers of interest.
  --
  -- There are two ways to ask for object identifiers, blocking and
  -- non-blocking. They otherwise have the same semantics.
  --
  -- The protocol maintains a notional FIFO of "outstanding" object
  -- identifiers that have been provided but not yet acknowledged. Only
  -- objects that are outstanding can be requested: they can be
  -- requested in any order, but at most once. Object identifiers are
  -- acknowledged in the same FIFO order they were provided in. The
  -- acknowledgement is included in the same messages used to ask for more
  -- object identifiers.
  data Message (SigSubmissionV2 sigId sig) from to where

    -- | Request a list of identifiers from the server, and confirm a
    -- number of outstanding identifiers.
    --
    -- With 'TokBlocking' this is a blocking operation but it's not guaranteed
    -- that the server will respond with signatures.  The server might block for
    -- only a limited time waiting for signaures, if it times out it will reply
    -- with `MsgReplyNoSigs` to let the client regain control of the protocol.
    --
    -- With 'TokNonBlocking' this is a non-blocking operation: the response may
    -- be an empty list and this does expect a prompt response. This covers high
    -- throughput use cases where we wish to pipeline, by interleaving requests
    -- for additional identifiers with requests for signatures, which
    -- requires these requests not block.
    --
    -- The request gives the maximum number of identifiers that can be
    -- accepted in the response. This must be greater than zero in the
    -- 'TokBlocking' case. In the 'TokNonBlocking' case either the numbers
    -- acknowledged or the number requested __MUST__ be non-zero. In either
    -- case, the number requested __MUST__ not put the total outstanding over
    -- the fixed protocol limit.
    --
    -- The request also gives the number of outstanding identifiers that
    -- can now be acknowledged. The actual signatures to acknowledge are known
    -- to the server based on the FIFO order in which they were provided.
    --
    -- There is no choice about when to use the blocking case versus the
    -- non-blocking case, it depends on whether there are any remaining
    -- unacknowledged signatures (after taking into account the ones
    -- acknowledged in this message):
    --
    -- * The blocking case __MUST__ be used when there are zero remaining
    --   unacknowledged signatures.
    --
    -- * The non-blocking case __MUST__ be used when there are non-zero
    --   remaining unacknowledged signatures.
    --
    MsgRequestSigIds
      :: forall (blocking :: StBlockingStyle) sigId sig.
         SingBlockingStyle blocking
      -> NumIdsAck -- ^ Acknowledge this number of outstanding signatures
      -> NumIdsReq -- ^ Request up to this number of identifiers
      -> Message (SigSubmissionV2 sigId sig) StIdle (StSigIds blocking)

    -- | Reply with a list of object identifiers for available objects, along
    -- with the size of each object.
    --
    -- The list must not be longer than the maximum number requested.
    --
    -- In the 'StSigIds' 'Blocking' state the list must be non-empty while in
    -- the 'StSigIds' 'NonBlocking' state the list may be empty.
    --
    -- These objects are added to the notional FIFO of outstanding object
    -- identifiers for the protocol.
    --
    -- The order in which these object identifiers are returned must be the
    -- order in which they are submitted to the mempool, to preserve dependent
    -- objects.
    --
    MsgReplySigIds
      :: BlockingReplyList blocking (sigId, SizeInBytes)
      -> Message (SigSubmissionV2 sigId sig) (StSigIds blocking) StIdle

    -- | The blocking request `MsgRequestSigIds` can be replied with no
    -- signatures to let the client regain the control of the protocol.
    --
    MsgReplyNoSigIds
      :: Message (SigSubmissionV2 sidId sig) (StSigIds StBlocking) StIdle

    -- | Request one or more objects corresponding to the given identifiers.
    --
    -- While it is the responsibility of the server to keep within
    -- pipelining in-flight limits, the client must also cooperate by keeping
    -- the total requested across all in-flight requests within the limits.
    --
    -- It is an error to ask for identifiers that were not
    -- previously announced (via 'MsgReplySigIds').
    --
    -- It is an error to ask for identifiers that are not
    -- outstanding or that were already asked for.
    --
    MsgRequestSigs
      :: [sigId]
      -> Message (SigSubmissionV2 sigId sig) StIdle StSigs

    -- | Reply with the requested signatures, or implicitly discard.
    --
    -- Signatures can become invalid between the time the identifier was
    -- sent and the signatures being requested. Invalid (including committed)
    -- signatures do not need to be sent.
    --
    -- Any identifiers requested but not provided in this reply
    -- should be considered as if this peer had never announced them. (Note
    -- that this is no guarantee that the signature is invalid, it may still be
    -- valid and available from another peer).
    --
    MsgReplySigs
      :: [sig]
      -> Message (SigSubmissionV2 sigId sig) StSigs StIdle

    -- | Termination message, initiated by the client side when idle.
    MsgDone
      :: Message (SigSubmissionV2 sigId sig) StIdle StDone

  type StateAgency StIdle       = ClientAgency
  type StateAgency (StSigIds b) = ServerAgency
  type StateAgency StSigs       = ServerAgency
  type StateAgency StDone       = NobodyAgency

  type StateToken = SingSigSubmissionV2

instance ( NFData sigId
         , NFData sig
         )
      => NFData (Message (SigSubmissionV2 sigId sig) from to) where
  rnf (MsgRequestSigIds tkbs w1 w2) = rnf tkbs `seq` rnf w1 `seq` rnf w2
  rnf (MsgReplySigIds brl)          = rnf brl
  rnf MsgReplyNoSigIds              = ()
  rnf (MsgRequestSigs sigIds)       = rnf sigIds
  rnf (MsgReplySigs sigs)           = rnf sigs
  rnf MsgDone                       = ()

deriving instance (Eq sigId, Eq sig)
               => Eq (Message (SigSubmissionV2 sigId sig) from to)

deriving instance (Show sigId, Show sig)
               => Show (Message (SigSubmissionV2 sigId sig) from to)
