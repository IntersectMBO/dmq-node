{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module DMQ.Diffusion.NodeKernel.Types
  ( NodeKernel (..)
  , Readiness (..)
  , PoolId
  , StakePools (..)
  , PoolValidationCtx (..)
  , ValidationCfg (..)
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI

import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.OrdPSQ (OrdPSQ)
import Data.Word
import System.Random (StdGen)

import Cardano.Ledger.Api.State.Query qualified as LedgerQuery

import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.KeepAlive (KeepAliveRegistry)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot,
           LedgerPeersKind (..))
import Ouroboros.Network.PeerSharing (PeerSharingAPI, PeerSharingRegistry)
import Ouroboros.Network.TxSubmission.Inbound.V2
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool (..))

import DMQ.Diffusion.PeerSelection.PeerMetric (PeerMetric)
import DMQ.Protocol.SigSubmission.Type (PoolId, Sig, SigId)


-- | A signal when a dmq-node becomes ready to validate signatures, e.g. when
-- local-state query returned necessary context.
--
data Readiness = NotReady | Ready
  deriving Show


data NodeKernel crypto ntnAddr m =
  NodeKernel {
    -- | The keep alive registry, used for the keep alive clients.
    keepAliveRegistry   :: !(KeepAliveRegistry (ConnectionId ntnAddr) m)

    -- | Read the current peer sharing registry, used for interacting with
    -- the PeerSharing protocol
  , peerSharingRegistry       :: !(PeerSharingRegistry ntnAddr m)
  , peerSharingAPI            :: !(PeerSharingAPI ntnAddr StdGen m)
  , peerSelectionPolicyRngVar :: !(StrictTVar m StdGen)
  , mempool                   :: !(Mempool m SigId (Sig crypto))
  , sigSharedTxStateVar       :: !(SharedTxStateVar m ntnAddr SigId)
  , sigPeerRegistry           :: !(PeerTxRegistry m ntnAddr)
  , sigRetiredCountersVar     :: !(TxSubmissionCountersVar m)
  , stakePools                :: !(StakePools m)
  , readinessVar              :: !(StrictTVar m Readiness)
  , peerMetric                :: !(PeerMetric m SigId ntnAddr)
  , lastSigByPoolIdVar        :: !(StrictTVar m (OrdPSQ PoolId Time ()))
  }

data StakePools m = StakePools {
    -- | contains map of cardano pool stake snapshot obtained
    -- via local state query client
    --
    -- NOTE: StakeSnapshot is taken from `VolatileTip`, this means that
    -- `ssMarkSet` is not safe to be used as we could be on an adversarial fork
    -- which crossed boundary.
    stakePoolsVar
      :: !(StrictTVar m (Map PoolId LedgerQuery.StakeSnapshot))

    -- | Acquire and update validation context for signature validation
    --
    -- First argument is the current time inserted into `PoolValidationCtx`,
    -- other fields are read from share state available in `STM` action.
  , withPoolValidationCtx
      :: forall a. UTCTime -> Time -> (PoolValidationCtx -> (a, PoolValidationCtx)) ->  STM m a

     -- | provides only those big peers which provide SRV endpoints
     -- as otherwise those are cardano-nodes
  , ledgerBigPeersVar
      :: !(StrictTVar m (Maybe (LedgerPeerSnapshot BigLedgerPeers)))

    -- | all ledger peers, restricted to srv endpoints
  , ledgerPeersVar
      :: !(StrictTMVar m (LedgerPeerSnapshot AllLedgerPeers))
  }

data PoolValidationCtx =
  PoolValidationCtx {
      vctxUTCNow          :: !UTCTime
      -- ^ current UTC time
    , vctxNow             :: !Time
      -- ^ current time
    , vctxReadiness       :: !Readiness
      -- ^ if pool validation ctx was initialised by local state query
      -- mini-prototocol
    , vctxStakeMap        :: !(Map PoolId LedgerQuery.StakeSnapshot)
      -- ^ for signature validation
    , vctxOcertMap        :: !(Map PoolId Word64)
      -- ^ ocert counters to check monotonicity
    , vctxPraosMaxKESEvo  :: !Word64
      -- ^ maximum number of KES iterations (read from the Shelley genesis file)
    , vctxLastSigByPoolId :: !(OrdPSQ PoolId Time ())
      -- ^ last valid signature by PoolId, to limit frequency of submitted
      -- signatures
    }
  deriving Show


newtype ValidationCfg = ValidationCfg {
    vcMinSigDelay :: DiffTime
    -- ^ minimal delay between signatures created by the same SPO.  It can be
    -- modified with an internal CLI option.  The default value is
    -- `DMQ.Policy.minSigDelay`.
  }
  deriving Eq

instance Aeson.ToJSON ValidationCfg where
  toJSON ValidationCfg { vcMinSigDelay } =
    Aeson.object [ "minSigDelay" Aeson..= vcMinSigDelay ]
