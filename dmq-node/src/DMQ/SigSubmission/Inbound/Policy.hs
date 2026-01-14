{-# LANGUAGE NumericUnderscores #-}

module DMQ.SigSubmission.Inbound.Policy
  ( SigDecisionPolicy (..)
  , defaultSigDecisionPolicy
  , max_SIG_SIZE
    -- * Re-exports
  , NumIdsReq (..)
  ) where

import Control.DeepSeq
import Control.Monad.Class.MonadTime.SI
import Ouroboros.Network.SizeInBytes (SizeInBytes (..))

import DMQ.Protocol.SigSubmissionV2.Type (NumIdsReq)


-- | Maximal sig size.
--
-- Affects:
--
-- * `SigDecisionPolicy`
-- * `maximumIngressQueue` for `sig-submission` mini-protocol, see
--   `DMQ.NodeToNode.sigSubmissionProtocolLimits`
--
max_SIG_SIZE :: SizeInBytes
max_SIG_SIZE = 65_540


-- | Policy for making decisions
--
data SigDecisionPolicy = SigDecisionPolicy {
      maxNumSigIdsToRequest   :: !NumIdsReq,
      -- ^ a maximal number of sigids requested at once.

      maxUnacknowledgedSigIds :: !NumIdsReq,
      -- ^ maximal number of unacknowledgedSigIds.  Measured in `NumIdsReq`
      -- since we enforce this policy by requesting not more sigids than what
      -- this limit allows.

      --
      -- Configuration of sig decision logic.
      --

      sigsSizeInflightPerPeer :: !SizeInBytes,
      -- ^ a limit of sig size in-flight from a single peer.
      -- It can be exceed by max sig size.

      maxSigsSizeInflight     :: !SizeInBytes,
      -- ^ a limit of sig size in-flight from all peers.
      -- It can be exceed by max sig size.

      sigInflightMultiplicity :: !Int,
      -- ^ from how many peers download the `sigid` simultaneously

      bufferedSigsMinLifetime :: !DiffTime,
      -- ^ how long sigs that have been added to the mempool will be
      -- kept in the `bufferedSigs` cache.

      scoreRate              :: !Double,
      -- ^ rate at which "rejected" sigs drain. Unit: sig/seconds.

      scoreMax               :: !Double
      -- ^ Maximum number of "rejections". Unit: seconds

    }
  deriving Show

instance NFData SigDecisionPolicy where
  rnf SigDecisionPolicy{} = ()

defaultSigDecisionPolicy :: SigDecisionPolicy
defaultSigDecisionPolicy =
  SigDecisionPolicy {
    maxNumSigIdsToRequest   = 3,
    maxUnacknowledgedSigIds = 10, -- must be the same as sigSubmissionMaxUnacked
    sigsSizeInflightPerPeer = max_SIG_SIZE * 6,
    maxSigsSizeInflight     = max_SIG_SIZE * 20,
    sigInflightMultiplicity = 2,
    bufferedSigsMinLifetime = 2,
    scoreRate               = 0.1,
    scoreMax                = 15 * 60
  }
