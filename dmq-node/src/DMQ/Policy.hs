{-# LANGUAGE NumericUnderscores #-}

module DMQ.Policy
  ( sigDecisionPolicy
  , sigSubmissionProtocolLimits
  , keepAliveProtocolLimits
  , peerSharingProtocolLimits
  , localProtocolLimits
  , peerMetricConfiguration
  , maxSigSize
  , minSigBodySize
  , maxSigBodySize
  , minSigDelay
  , maxSigExpiresAtDelay
  , maxSigIdsInflight
  , cardanoEpochSlots
  , defaultValidationCfg
  , dmqMainnetNetworkMagic
  ) where

import Data.Time (DiffTime, NominalDiffTime)

import Cardano.Chain.Slotting (EpochSlots (..))

import DMQ.Diffusion.NodeKernel.Types (ValidationCfg (..))
import DMQ.Diffusion.PeerSelection.PeerMetric (PeerMetricConfiguration (..))
import DMQ.NodeToNode.Version (NodeToNodeVersion (..))
import DMQ.Protocol.SigSubmission.Type (NumTxIdsToReq)

import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (DirectionalLimits (..), MiniProtocolLimits (..),
           symmetricLimits)
import Ouroboros.Network.Protocol.Limits (smallByteLimit)
import Ouroboros.Network.SizeInBytes (SizeInBytes)
import Ouroboros.Network.TxSubmission.Inbound.V2

-- | Maximum signature size.
-- TODO: make it configurable
--
-- See: https://cips.cardano.org/cip/CIP-0137#mithril-extra-network-usage
maxSigSize :: SizeInBytes
maxSigSize = 2800

-- | Minimum `sigBody` size
--
-- See: https://cips.cardano.org/cip/CIP-0137#mithril-extra-network-usage
minSigBodySize :: SizeInBytes
minSigBodySize = 90

-- | Maximum `sigBody` size, it must be smaller than `maxSigSize`.
--
-- See: https://cips.cardano.org/cip/CIP-0137#mithril-extra-network-usage
maxSigBodySize :: SizeInBytes
maxSigBodySize = 2000


-- | Minimum distance between `sigExpiresAt` of two signatures issued by the
-- same `PoolId`, see `DMQ.Protocol.SigSubmission.SigSpacing`.
--
-- A pool can have at most @maxSigExpiresAtDelay / minSigDelay@ (@30@)
-- signatures which have not expired.
--
minSigDelay :: DiffTime
minSigDelay = 60

-- | We bound the `sigExpiresAt` to `maxSigExpiersAtDelay` seconds into the
-- future.
--
maxSigExpiresAtDelay :: NominalDiffTime
maxSigExpiresAtDelay = 1800


defaultValidationCfg :: ValidationCfg
defaultValidationCfg = ValidationCfg {
    vcMinSigDelay = minSigDelay
  }


-- | Maximum numbers signatures in-flight per peer.
--
-- NOTE: it is used by:
-- * `sigDecisionPolicy`
-- * `byteLimitsSigSubmission`,
-- * `byteLimitsSigSubmissionV2`, and
-- * `sigSubmissionProtocolLimits`
--
-- NOTE: Since `maxSigsInflight` or `maxSigSize` are used by the codec size
-- limits and ingress limit, changes might require a new `node-to-node`
-- protocol version.
--
-- TODO: make it configurable
maxSigIdsInflight :: NumTxIdsToReq
maxSigIdsInflight = 33

-- | The `TxDecisionPolicy` used by `SigSubmission`.
--
-- * `maxUnacknowledgedTxIds` is 4 times `maxSigsInflight` (@132@).
-- * `txsSizeInflightPerPeer` is `maxSigSize * maxSigsInflight` (@92.4kB@).
-- * By default there are 25 hot peers, so at most
--   @
--    25 * txsSizeInflightPerPeer
--   @
--   bytes (@2310kB@) in-flight for NodeToNodeV_2 or higher.
--
sigDecisionPolicy :: TxDecisionPolicy
sigDecisionPolicy = TxDecisionPolicy {
    maxNumTxIdsToRequest           = maxSigIdsInflight,
    maxUnacknowledgedTxIds         = 4 * maxSigIdsInflight,
    txsSizeInflightPerPeer         = maxSigSize * fromIntegral maxSigIdsInflight,
    maxOutstandingTxBatchesPerPeer = 4,
    txInflightMultiplicity         = 1,
    bufferedTxsMinLifetime         = 0,
    scoreRate                      = 0.1,
    scoreMax                       = 15 * 60,
    scoreAcceptDecrement           = 3,
    interTxSpace                   = 0.250,
    inflightTimeout                = 0.600,
    maxPeerClaimDelay              = 0.250,
    disablePipelinedTxIdRequests   = False
  }



-- | V1 delivers signatures to the responder and V2 delivers them to the
-- initiator. The peer serving signatures receives requests for IDs and bodies.
sigSubmissionProtocolLimits :: NodeToNodeVersion -> DirectionalLimits
sigSubmissionProtocolLimits = \case
    NodeToNodeV_1 -> DirectionalLimits {
        initiatorLimits = requestLimits,
        responderLimits = replyLimits
      }
    -- `NodeToNodeV_2` swapped the client/server roles
    NodeToNodeV_2 -> DirectionalLimits {
        initiatorLimits = replyLimits,
        responderLimits = requestLimits
      }
  where
    -- A singleton body request takes 38 bytes including the 34-byte SigId.
    -- Separate requests to acknowledge one ID and request another take
    -- 5 bytes each. All 132 outstanding IDs fit in 6,336 bytes this way.
    requestLimits = ingressLimitWithMargin (fromIntegral smallByteLimit)

    -- Allow for all signature bodies in flight from this peer.
    replyLimits = ingressLimitWithMargin (fromIntegral (txsSizeInflightPerPeer sigDecisionPolicy))

-- | Keep-alive has one small message outstanding in either direction.
keepAliveProtocolLimits :: DirectionalLimits
keepAliveProtocolLimits = symmetricLimits (ingressLimitWithMargin 1280)

-- | A peer-sharing reply can list up to 255 peers. Allow four TCP segments
-- for the reply and one segment plus a margin for the small, unpipelined request.
peerSharingProtocolLimits :: DirectionalLimits
peerSharingProtocolLimits = DirectionalLimits {
    initiatorLimits = MiniProtocolLimits { maximumIngressQueue = 4 * 1440 },
    responderLimits = ingressLimitWithMargin 1440
  }

-- | Local socket protocols use a 32-bit byte limit in both directions,
-- capped at the largest queue size the platform can represent.
localProtocolLimits :: DirectionalLimits
localProtocolLimits = symmetricLimits MiniProtocolLimits {
    maximumIngressQueue = fromInteger (min 0xffffffff (toInteger (maxBound :: Int)))
  }

ingressLimitWithMargin :: Int -> MiniProtocolLimits
ingressLimitWithMargin bytes = MiniProtocolLimits {
    maximumIngressQueue = bytes + bytes `div` 10
  }


peerMetricConfiguration :: PeerMetricConfiguration
peerMetricConfiguration = PeerMetricConfiguration { timeWindowToKeep = 3600 }


-- Cardano node sets `epochSlots` by:
--
-- * asserting that Byron security parameter and Shelley security
--   parameter are equal, see
--   https://github.com/intersectmbo/ouroboros-consensus/tree/release-ouroboros-consensus-3.0.1.0/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L850-L851
--
-- * multiplies it by the factor of `10`: see
--   https://github.com/IntersectMBO/cardano-api/tree/cardano-rpc-11.0.0.0/cardano-api/src/Cardano/Api/LedgerState.hs#L525
--
-- We hard code it, since the local state query is only using it for decoding
-- Byron blocks which dmq-node doesn't need to know about at all.
--
cardanoEpochSlots :: EpochSlots
cardanoEpochSlots = EpochSlots $ 10 * k
  where
    k = 2160


-- | Mainnet `NetworkMagic` for dmq according to CIP#0137.
--
dmqMainnetNetworkMagic :: NetworkMagic
dmqMainnetNetworkMagic = NetworkMagic 2_912_307_721
