{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module DMQ.SigSubmission.Inbound.Types
  ( -- * PeerSigState
    PeerSigState (..)
    -- * SharedSigState
  , SharedSigState (..)
    -- * Decisions
  , TxsToMempool (..)
  , SigDecision (..)
  , emptySigDecision
  , TraceSigLogic (..)
  , SigSubmissionInitDelay (..)
  , defaultSigSubmissionInitDelay
  
    -- * Types shared with V1
    -- ** Various
  , ProcessedTxCount (..)
    -- ** Traces
  , TraceSigSubmissionInbound (..)
  , TxSubmissionCounters (..)
  , mkTxSubmissionCounters
    -- ** Protocol Error
  , TxSubmissionProtocolError (..)
  ) where

import Control.DeepSeq
import Control.Monad.Class.MonadTime.SI
import Data.Aeson (ToJSON (toJSON), Value (String), object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack)
import GHC.Generics (Generic)
import System.Random (StdGen)

import NoThunks.Class (NoThunks (..))

import Ouroboros.Network.ControlMessage (ControlMessage)
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V2 (ProcessedTxCount (..),
                   TxsToMempool (..), TxSubmissionCounters (..),
                   TxSubmissionProtocolError (..))

import DMQ.Protocol.SigSubmissionV2.Type (NumIdsAck, NumIdsReq)

--
-- PeerSigState, SharedSigState
--

data PeerSigState sigid sig = PeerSigState {
       -- | Those signatures (by their identifier) that the client has told
       -- us about, and which we have not yet acknowledged. This is kept in
       -- the order in which the client gave them to us. This is the same order
       -- in which we submit them to the mempool (or for this example, the final
       -- result order). It is also the order we acknowledge in.
       --
       unacknowledgedSigIds      :: !(StrictSeq sigid),

       -- | Set of known signature ids which can be requested from this peer.
       --
       availableSigIds           :: !(Map sigid SizeInBytes),

       -- | The number of signature identifiers that we have requested but
       -- which have not yet been replied to. We need to track this it keep
       -- our requests within the limit on the number of unacknowledged sigids.
       --
       requestedSigIdsInflight   :: !NumIdsReq,

       -- | The size in bytes of signatures that we have requested but which
       -- have not yet been replied to. We need to track this to keep our
       -- requests within the `maxSigsSizeInflight` limit.
       --
       requestedSigsInflightSize :: !SizeInBytes,

       -- | The set of requested `sigid`s.
       --
       requestedSigsInflight     :: !(Set sigid),

       -- | A subset of `unacknowledgedSigIds` which were unknown to the peer
       -- (i.e. requested but not received). We need to track these `sigid`s
       -- since they need to be acknowledged.
       --
       -- We track these `sigid` per peer, rather than in `bufferedSigs` map,
       -- since that could potentially lead to corrupting the node, not being
       -- able to download a `sig` which is needed & available from other nodes.
       --
       unknownSigs               :: !(Set sigid),

       -- | Score is a metric that tracks how usefull a peer has been.
       -- The larger the value the less usefull peer. It slowly decays towards
       -- zero.
       score                    :: !Double,

       -- | Timestamp for the last time `score` was drained.
       scoreTs                  :: !Time,

       -- | A set of sigs downloaded from the peer. They are not yet
       -- acknowledged and haven't been sent to the mempool yet.
       --
       -- Life cycle of entries:
       -- * added when a sig is downloaded (see `collectSigsImpl`)
       -- * follows `unacknowledgedSigIds` (see `acknowledgeSigIds`)
       --
       downloadedSigs            :: !(Map sigid sig),

       -- | A set of sigs on their way to the mempool.
       -- Tracked here so that we can cleanup `inSubmissionToMempoolSigs` if the
       -- peer dies.
       --
       -- Life cycle of entries:
       -- * added by `acknowledgeSigIds` (where decide which sigs can be
       --   submitted to the mempool)
       -- * removed by `withMempoolSem`
       --
       toMempoolSigs             :: !(Map sigid sig)

    }
    deriving (Eq, Show, Generic, NFData)

instance ( NoThunks sigid
         , NoThunks sig
         ) => NoThunks (PeerSigState sigid sig)


-- | Shared state of all `SigSubmission` clients.
--
-- New `sigid` enters `unacknowledgedSigIds` it is also added to `availableSigIds`
-- and `referenceCounts` (see `acknowledgeTxIdsImpl`).
--
-- When a `sigid` id is selected to be downloaded, it's added to
-- `requestedSigsInflightSize` (see
-- `DMQ.SigSubmission.Inbound.Decision.pickSigsToDownload`).
--
-- When the request arrives, the `sigid` is removed from `inflightSigs`.  It
-- might be added to `unknownSigs` if the server didn't have that `sigid`, or
-- it's added to `bufferedSigs` (see `collectSigsImpl`).
--
-- Whenever we choose `sigid` to acknowledge (either in `acknowledtxsIdsImpl`,
-- `collectSigsImpl` or
-- `DMQ.SigSubmission.Inbound.Decision.pickSigsToDownload`, we also
-- recalculate `referenceCounts` and only keep live `sigid`s in other maps (e.g.
-- `availableSigIds`, `bufferedSigs`, `unknownSigs`).
--
data SharedSigState peeraddr sigid sig = SharedSigState {

      -- | Map of peer states.
      --
      -- /Invariant:/ for peeraddr's which are registered using `withPeer`,
      -- there's always an entry in this map even if the set of `sigid`s is
      -- empty.
      --
      peerSigStates             :: !(Map peeraddr (PeerSigState sigid sig)),

      -- | Set of signatures which are in-flight (have already been
      -- requested) together with multiplicities (from how many peers it is
      -- currently in-flight)
      --
      -- This set can intersect with `availableSigIds`.
      --
      inflightSigs              :: !(Map sigid Int),

      -- | Overall size of all `sig`s in-flight.
      --
      inflightSigsSize          :: !SizeInBytes,

      -- | Map of `sig` which:
      --
      --    * were downloaded and added to the mempool,
      --    * are already in the mempool (`Nothing` is inserted in that case),
      --
      -- We only keep live `sigid`, e.g. ones which `sigid` is unacknowledged by
      -- at least one peer or has a `timedSigs` entry.
      --
      -- /Note:/ `sigid`s which `sig` were unknown by a peer are tracked
      -- separately in `unknownSigs`.
      --
      -- /Note:/ previous implementation also needed to explicitly track
      -- `sigid`s which were already acknowledged, but are still unacknowledged.
      -- In this implementation, this is done using reference counting.
      --
      -- This map is useful to acknowledge `sigid`s, it's basically taking the
      -- longest prefix which contains entries in `bufferedSigs` or `unknownSigs`.
      --
      bufferedSigs              :: !(Map sigid (Maybe sig)),

      -- | We track reference counts of all unacknowledged and timedSigs sigids.
      -- Once the count reaches 0, a sig is removed from `bufferedSigs`.
      --
      -- The `bufferedSig` map contains a subset of `sigid` which
      -- `referenceCounts` contains.
      --
      -- /Invariants:/
      --
      --    * the sigid count is equal to multiplicity of sigid in all
      --      `unacknowledgedSigIds` sequences;
      --    * @Map.keysSet bufferedSigs `Set.isSubsetOf` Map.keysSet referenceCounts@;
      --    * all counts are positive integers.
      --
      referenceCounts           :: !(Map sigid Int),

      -- | A set of timeouts for sigids that have been added to bufferedSigs after being
      -- inserted into the mempool.
      --
      -- We need these short timeouts to avoid re-downloading a `sig`.  We could
      -- acknowledge this `sigid` to all peers, when a peer from another
      -- continent presents us it again.
      --
      -- Every sigid entry has a reference count in `referenceCounts`.
      --
      timedSigs                 :: !(Map Time [sigid]),

      -- | A set of sigids that have been downloaded by a peer and are on their
      -- way to the mempool. We won't issue further fetch-requests for sigs in
      -- this state.  We track these sigs to not re-download them from another
      -- peer.
      --
      -- * We subtract from the counter when a given sig is added or rejected by
      --   the mempool or do that for all sigs in `toMempoolTxs` when a peer is
      --   unregistered.
      -- * We add to the counter when a given sig is selected to be added to the
      --   mempool in `pickSigsToDownload`.
      --
      inSubmissionToMempoolSigs :: !(Map sigid Int),

      -- | Rng used to randomly order peers
      peerRng                  :: !StdGen
    }
    deriving (Eq, Show, Generic, NFData)

instance ( NoThunks peeraddr
         , NoThunks sig
         , NoThunks sigid
         , NoThunks StdGen
         ) => NoThunks (SharedSigState peeraddr sigid sig)


-- | Decision made by the decision logic.  Each peer will receive a 'Decision'.
--
-- /note:/ it is rather non-standard to represent a choice between requesting
-- `sigid`s and `sig`'s as a product rather than a sum type.  The client will
-- need to download `sig`s first and then send a request for more sigids (and
-- acknowledge some `sigid`s).   Due to pipelining each client will request
-- decision from the decision logic quite often (every two pipelined requests),
-- but with this design a decision once taken will make the peer non-active
-- (e.g. it won't be returned by `filterActivePeers`) for longer, and thus the
-- expensive `makeDecision` computation will not need to take that peer into
-- account.
--
data SigDecision sigid sig = SigDecision {
    sigdSigIdsToAcknowledge :: !NumIdsAck,
    -- ^ id's to acknowledge

    sigdSigIdsToRequest     :: !NumIdsReq,
    -- ^ number of id's to request

    sigdPipelineSigIds      :: !Bool,
    -- ^ the sig-submission protocol only allows to pipeline `sigid`'s requests
    -- if we have non-acknowledged `sigid`s.

    sigdSigsToRequest       :: !(Map sigid SizeInBytes),
    -- ^ sigid's to download.

    sigdSigsToMempool       :: !(TxsToMempool sigid sig)
    -- ^ list of `sig`s to submit to the mempool.
  }
  deriving (Show, Eq)

instance (NFData sigid, NFData sig) => NFData (SigDecision sigid sig) where
  -- all fields except `sigdSigsToMempool` when evaluated to WHNF evaluate to NF.
  rnf SigDecision {sigdSigsToMempool} = rnf sigdSigsToMempool

-- | A non-commutative semigroup instance.
--
-- /note:/ this instance must be consistent with `pickSigsToDownload` and how
-- `PeerSigState` is updated.  It is designed to work with `TMergeVar`s.
--
instance Ord sigid => Semigroup (SigDecision sigid sig) where
    SigDecision { sigdSigIdsToAcknowledge,
                  sigdSigIdsToRequest,
                  sigdPipelineSigIds = _ignored,
                  sigdSigsToRequest,
                  sigdSigsToMempool }
      <>
      SigDecision { sigdSigIdsToAcknowledge = sigdSigIdsToAcknowledge',
                    sigdSigIdsToRequest     = sigdSigIdsToRequest',
                    sigdPipelineSigIds      = sigdPipelineSigIds',
                    sigdSigsToRequest       = sigdSigsToRequest',
                    sigdSigsToMempool       = sigdSigsToMempool' }
      =
      SigDecision { sigdSigIdsToAcknowledge = sigdSigIdsToAcknowledge + sigdSigIdsToAcknowledge',
                    sigdSigIdsToRequest     = sigdSigIdsToRequest + sigdSigIdsToRequest',
                    sigdPipelineSigIds      = sigdPipelineSigIds',
                    sigdSigsToRequest       = sigdSigsToRequest <> sigdSigsToRequest',
                    sigdSigsToMempool       = sigdSigsToMempool <> sigdSigsToMempool'
                 }

-- | A no-op decision.
emptySigDecision :: SigDecision sigid sig
emptySigDecision = SigDecision {
    sigdSigIdsToAcknowledge = 0,
    sigdSigIdsToRequest     = 0,
    sigdPipelineSigIds      = False,
    sigdSigsToRequest       = Map.empty,
    sigdSigsToMempool       = mempty
  }


-- | SigLogic tracer.
--
data TraceSigLogic peeraddr sigid sig =
    TraceSharedTxState String (SharedSigState peeraddr sigid sig)
  | TraceSigDecisions (Map peeraddr (SigDecision sigid sig))
  deriving Show

instance ( Show addr
         , Show sigid
         , Show sig
         )
       => ToJSON (TraceSigLogic addr sigid sig) where
  toJSON (TraceSharedTxState tag st) =
    object [ "kind" .= String "SharedSigState"
           , "tag"  .= String (pack tag)
           , "sharedSigState" .= String (pack . show $ st)
           ]
  toJSON (TraceSigDecisions decisions) =
    object [ "kind"      .= String "SigDecisions"
           , "decisions" .= String (pack . show $ decisions)
           ]


data TraceSigSubmissionInbound sigid sig =
    -- | Number of signatures just about to be inserted.
    TraceSigSubmissionCollected [sigid]
    -- | Just processed signature pass/fail breakdown.
  | TraceSigSubmissionProcessed ProcessedTxCount
  | TraceSigInboundCanRequestMoreSigs Int
  | TraceSigInboundCannotRequestMoreSigs Int
  | TraceSigInboundAddedToMempool [sigid] DiffTime
  | TraceSigInboundRejectedFromMempool [sigid] DiffTime
  | TraceSigInboundError TxSubmissionProtocolError

  --
  -- messages emitted by the new implementation of the server in
  -- "DMQ.SigSubmission.Outbound"; some of them are also
  -- used in this module.
  --

  -- | Server received 'MsgDone'
  | TraceSigInboundTerminated
  | TraceSigInboundDecision (SigDecision sigid sig)
  | TraceControlMessage ControlMessage
  deriving (Eq, Show)

instance ( ToJSON sigid
         , ToJSON sig
         , Show sigid
         , Show sig
         )
       => ToJSON (TraceSigSubmissionInbound sigid sig) where
  toJSON (TraceSigSubmissionCollected count) =
    object
      [ "kind" .= String "SigSubmissionCollected"
      , "count" .= toJSON count
      ]
  toJSON (TraceSigSubmissionProcessed processed) =
    object
      [ "kind" .= String "SigSubmissionProcessed"
      , "accepted" .= toJSON (ptxcAccepted processed)
      , "rejected" .= toJSON (ptxcRejected processed)
      ]
  toJSON (TraceSigInboundCanRequestMoreSigs count) =
    object
      [ "kind" .= String "SigInboundCanRequestMoreSigs"
      , "count" .= toJSON count
      ]
  toJSON (TraceSigInboundCannotRequestMoreSigs count) =
    object
      [ "kind" .= String "SigInboundCannotRequestMoreSigs"
      , "count" .= toJSON count
      ]
  toJSON (TraceSigInboundAddedToMempool sigids diffTime') =
    object
      [ "kind" .= String "SigInboundAddedToMempool"
      , "sigids" .= toJSON sigids
      , "time" .= diffTime'
      ]
  toJSON (TraceSigInboundRejectedFromMempool sigids diffTime') =
    object
      [ "kind" .= String "SigInboundRejectedFromMempool"
      , "sigids" .= toJSON sigids
      , "time" .= diffTime'
      ]
  toJSON (TraceSigInboundError err) =
    object
      [ "kind" .= String "SigInboundError"
      , "error" .= String (pack $ show err)
      ]
  toJSON TraceSigInboundTerminated =
    object
      [ "kind" .= String "SigInboundTerminated"
      ]
  toJSON (TraceSigInboundDecision decision) =
    object
      [ "kind" .= String "SigInboundDecision"
      -- TODO: this is too verbose, it will show full sig's
      , "decision" .= String (pack $ show decision)
      ]
  toJSON (TraceControlMessage controlMessage) =
    object
      [ "kind" .= String "ControlMessage"
      , "controlMessage" .= String (pack $ show controlMessage)
      ]


mkTxSubmissionCounters
  :: Ord sigid
  => SharedSigState peeraddr sigid sig
  -> TxSubmissionCounters
mkTxSubmissionCounters
  SharedSigState {
    inflightSigs,
    bufferedSigs,
    referenceCounts,
    inSubmissionToMempoolSigs
  }
  =
  TxSubmissionCounters {
    numOfOutstandingTxIds         = Set.size $ Map.keysSet referenceCounts
                                        Set.\\ Map.keysSet bufferedSigs
                                        Set.\\ Map.keysSet inSubmissionToMempoolSigs,
    numOfBufferedTxs              = Map.size bufferedSigs,
    numOfInSubmissionToMempoolTxs = Map.size inSubmissionToMempoolSigs,
    numOfTxIdsInflight            = getSum $ foldMap Sum inflightSigs
  }


data SigSubmissionInitDelay =
     SigSubmissionInitDelay DiffTime
 | NoSigSubmissionInitDelay
 deriving (Eq, Show)

defaultSigSubmissionInitDelay :: SigSubmissionInitDelay
defaultSigSubmissionInitDelay = SigSubmissionInitDelay 60
