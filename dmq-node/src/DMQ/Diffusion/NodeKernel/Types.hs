{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module DMQ.Diffusion.NodeKernel.Types
  ( NodeKernel (..)
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

import Ouroboros.Network.BlockFetch (FetchClientRegistry)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot,
           LedgerPeersKind (..))
import Ouroboros.Network.PeerSharing (PeerSharingAPI, PeerSharingRegistry)
import Ouroboros.Network.TxSubmission.Inbound.V2
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool (..))

import DMQ.PeerSelection.PeerMetric (PeerMetrics)
import DMQ.Protocol.SigSubmission.Type (Sig, SigId)


data NodeKernel crypto ntnAddr m =
  NodeKernel {
    -- | The fetch client registry, used for the keep alive clients.
    fetchClientRegistry :: !(FetchClientRegistry (ConnectionId ntnAddr) () () m)

    -- | Read the current peer sharing registry, used for interacting with
    -- the PeerSharing protocol
  , peerSharingRegistry :: !(PeerSharingRegistry ntnAddr m)
  , peerSharingAPI      :: !(PeerSharingAPI ntnAddr StdGen m)
  , mempool             :: !(Mempool m SigId (Sig crypto))
  , sigChannelVar       :: !(TxChannelsVar m ntnAddr SigId (Sig crypto))
  , sigMempoolSem       :: !(TxMempoolSem m)
  , sigSharedTxStateVar :: !(SharedTxStateVar m ntnAddr SigId (Sig crypto))
  , stakePools          :: !(StakePools m)
  , nextEpochVar        :: !(StrictTVar m (Maybe UTCTime))
  , peerMetrics         :: !(PeerMetrics m SigId ntnAddr)
  }


-- | Cardano pool id's are hashes of the cold verification key
--
type PoolId = Ledger.KeyHash Ledger.StakePool

data StakePools m = StakePools {
    -- | contains map of cardano pool stake snapshot obtained
    -- via local state query client
    stakePoolsVar
      :: !(StrictTVar m (Map PoolId LedgerQuery.StakeSnapshot))
    -- | Acquire and update validation context for signature validation
  , withPoolValidationCtx
      :: forall a. (PoolValidationCtx -> (a, PoolValidationCtx)) ->  STM m a
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
      vctxEpoch    :: !(Maybe UTCTime)
      -- ^ UTC time of next epoch boundary for handling clock skew
    , vctxStakeMap :: !(Map PoolId LedgerQuery.StakeSnapshot)
      -- ^ for signature validation
    , vctxOcertMap :: !(Map PoolId Word64)
      -- ^ ocert counters to check monotonicity
    }
  deriving Show
