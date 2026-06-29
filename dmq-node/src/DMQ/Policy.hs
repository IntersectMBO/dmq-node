module DMQ.Policy
  ( sigDecisionPolicy
  , sigSubmissionIngressLimit
  , peerMetricConfiguration
  , maxSigSize
  , minSigBodySize
  , maxSigBodySize
  , minSigDelay
  , maxSigExpiresAtDelay
  , maxSigIdsInflight
  , cardanoEpochSlots
  ) where

import Data.Time (DiffTime, NominalDiffTime)

import Cardano.Chain.Slotting (EpochSlots (..))

import DMQ.Diffusion.PeerSelection.PeerMetric (PeerMetricConfiguration (..))
import DMQ.Protocol.SigSubmission.Type (NumTxIdsToReq)

import Network.Mux.Types (MiniProtocolLimits (..))

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


-- | Minimum delay between signatures created by the same `PoolId`.
--
minSigDelay :: DiffTime
minSigDelay = 60

-- | We bound the `sigExpiresAt` to `maxSigExpiersAtDelay` seconds into the
-- future.
--
maxSigExpiresAtDelay :: NominalDiffTime
maxSigExpiresAtDelay = 1800


-- | Maximum numbers signatures in-flight per peer.
--
-- NOTE: it is used by:
-- * `sigDecisionPolicy`
-- * `byteLimitsSigSubmission`,
-- * `byteLimitsSigSubmissionV2`, and
-- * `sigSubmissionIngressLimit`
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
    maxNumTxIdsToRequest   = maxSigIdsInflight,
    maxUnacknowledgedTxIds = 4 * maxSigIdsInflight,
    txsSizeInflightPerPeer = maxSigSize * fromIntegral maxSigIdsInflight,
    txInflightMultiplicity = 1,
    bufferedTxsMinLifetime = 0,
    scoreRate              = 0.1,
    scoreMax               = 15 * 60
  }


-- | Ingress limit for SigSubmission mini-protocol, derived from
-- `sigDecisionPolicy` (`txsSizeInflightPerPeer`).
--
sigSubmissionIngressLimit :: MiniProtocolLimits
sigSubmissionIngressLimit = MiniProtocolLimits {
    maximumIngressQueue = addMargin (fromIntegral sigSizeInflight)
  }
  where
    TxDecisionPolicy { txsSizeInflightPerPeer = sigSizeInflight }
      = sigDecisionPolicy

    addMargin :: Int -> Int
    addMargin = \x -> x + x `div` 10


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
