{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module DMQ.Diffusion.NodeKernel.Types
  ( NodeKernel (..)
  , Readiness (..)
  , PoolId
  , StakePools (..)
  , PoolValidationCtx (..)
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI

import Data.Map.Strict (Map)
import Data.Word
import System.Random (StdGen)

import Cardano.Ledger.Api.State.Query qualified as LedgerQuery
import Cardano.Ledger.Shelley.API qualified as Ledger

import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.KeepAlive (KeepAliveRegistry)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot,
           LedgerPeersKind (..))
import Ouroboros.Network.PeerSharing (PeerSharingAPI, PeerSharingRegistry)
import Ouroboros.Network.TxSubmission.Inbound.V2
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool (..))

import DMQ.Diffusion.PeerSelection.PeerMetric (PeerMetric)
import DMQ.Protocol.SigSubmission.Type (Sig, SigId)


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
  , peerSharingRegistry :: !(PeerSharingRegistry ntnAddr m)
  , peerSharingAPI      :: !(PeerSharingAPI ntnAddr StdGen m)
  , mempool             :: !(Mempool m SigId (Sig crypto))
  , sigChannelVar       :: !(TxChannelsVar m ntnAddr SigId (Sig crypto))
  , sigMempoolSem       :: !(TxMempoolSem m)
  , sigSharedTxStateVar :: !(SharedTxStateVar m ntnAddr SigId (Sig crypto))
  , stakePools          :: !(StakePools m)
  , readinessVar        :: !(StrictTVar m Readiness)
  , peerMetric          :: !(PeerMetric m SigId ntnAddr)
  }


-- | Cardano pool id's are hashes of the cold verification key
--
type PoolId = Ledger.KeyHash Ledger.StakePool

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
      :: forall a. UTCTime -> (PoolValidationCtx -> (a, PoolValidationCtx)) ->  STM m a

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
      vctxNow            :: UTCTime
      -- ^ current time
    , vctxReadiness      :: !Readiness
      -- ^ if pool validation ctx was initialised by local state query
      -- mini-prototocol
    , vctxStakeMap       :: !(Map PoolId LedgerQuery.StakeSnapshot)
      -- ^ for signature validation
    , vctxOcertMap       :: !(Map PoolId Word64)
      -- ^ ocert counters to check monotonicity
    , vctxPraosMaxKESEvo :: !Word64
      -- ^ maximum number of KES iterations (read from the Shelley genesis file)
    }
  deriving Show
