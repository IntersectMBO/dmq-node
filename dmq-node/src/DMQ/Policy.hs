{-# LANGUAGE NumericUnderscores #-}

module DMQ.Policy
  ( sigDecisionPolicy
  , sigSubmissionIngressLimit
  ) where

import DMQ.Protocol.SigSubmission.Type (NumTxIdsToReq)
import Network.Mux.Types (MiniProtocolLimits (..))
import Ouroboros.Network.SizeInBytes (SizeInBytes)
import Ouroboros.Network.TxSubmission.Inbound.V2

-- TODO: make it configurable
maxSigSize :: SizeInBytes
maxSigSize = 2800

-- | Maximum numbers signatures in-flight per peer.
--
-- TODO: make it configurable
maxSigsInflight :: NumTxIdsToReq
maxSigsInflight = 33

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
    maxNumTxIdsToRequest   = maxSigsInflight,
    maxUnacknowledgedTxIds = 4 * maxSigsInflight,
    txsSizeInflightPerPeer = maxSigSize * fromIntegral maxSigsInflight,
    maxTxsSizeInflight     = 250_000,
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

