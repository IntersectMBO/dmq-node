{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module DMQ.Diffusion.Applications where

import Control.Concurrent.Class.MonadSTM.Strict

import DMQ.Configuration
import DMQ.Diffusion.NodeKernel (NodeKernel (..))
import DMQ.Diffusion.PeerSelectionPolicy qualified as PeerSelectionPolicy
import DMQ.NodeToClient (NodeToClientVersion, NodeToClientVersionData,
           stdVersionDataNTC)
import DMQ.NodeToClient qualified as NTC
import DMQ.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           stdVersionDataNTN)
import DMQ.NodeToNode qualified as NTN

import Ouroboros.Network.Diffusion.Types qualified as Diffusion
import Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import Ouroboros.Network.Protocol.Handshake.Version (combineVersions,
           simpleSingletonVersions)
import Ouroboros.Network.RethrowPolicy (ioErrorRethrowPolicy,
           muxErrorRethrowPolicy)

diffusionApplications
  :: ( MonadSTM m
     , Ord ntnAddr
     )
  => NodeKernel crypto ntnAddr m
  -> Configuration
  -> Diffusion.Configuration NoExtraFlags m ntnFd ntnAddr ntcFd ntcAddr
  -> NTN.LimitsAndTimeouts crypto ntnAddr
  -> NTN.Apps ntnAddr m a ()
  -> NTC.Apps ntcAddr m ()
  -> Diffusion.Applications ntnAddr NodeToNodeVersion   NodeToNodeVersionData
                            ntcAddr NodeToClientVersion NodeToClientVersionData
                            NoExtraFlags m a
diffusionApplications
  NodeKernel {
    peerSharingRegistry,
    peerSelectionPolicyRngVar,
    peerMetric
  }
  Configuration {
    dmqcNetworkMagic = I networkMagic
  }
  Diffusion.Configuration {
    Diffusion.dcMode
  , Diffusion.dcPeerSharing
  }
  ntnLimitsAndTimeouts
  ntnApps
  ntcApps
  =
  Diffusion.Applications {
    Diffusion.daApplicationInitiatorMode =
      combineVersions
        [ simpleSingletonVersions
            version
            (stdVersionDataNTN networkMagic dcMode dcPeerSharing)
            (NTN.initiatorProtocols ntnLimitsAndTimeouts ntnApps version)
        | version <- [minBound..maxBound]
        ]
  , Diffusion.daApplicationInitiatorResponderMode =
      combineVersions
        [ simpleSingletonVersions
            version
            (stdVersionDataNTN networkMagic dcMode dcPeerSharing)
            (NTN.initiatorAndResponderProtocols ntnLimitsAndTimeouts ntnApps version)
        | version <- [minBound..maxBound]
        ]
  , Diffusion.daLocalResponderApplication =
      combineVersions
        [ simpleSingletonVersions
            version
            (stdVersionDataNTC networkMagic)
            (NTC.responders ntcApps version)
        | version <- [minBound..maxBound]
        ]
  , Diffusion.daRethrowPolicy       =  muxErrorRethrowPolicy
                                   <> ioErrorRethrowPolicy
  , Diffusion.daReturnPolicy        = const dmqRepromoteDelay
  , Diffusion.daRepromoteErrorDelay = dmqRepromoteDelay
  , Diffusion.daLocalRethrowPolicy  = mempty
  , Diffusion.daPeerSelectionPolicy = PeerSelectionPolicy.policy peerSelectionPolicyRngVar peerMetric
  , Diffusion.daPeerSharingRegistry = peerSharingRegistry
  }


-- | PeerSelection RepromoteDelay used after
dmqRepromoteDelay :: RepromoteDelay
dmqRepromoteDelay = 10
