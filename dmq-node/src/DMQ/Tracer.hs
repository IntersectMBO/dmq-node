{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DMQ.Tracer
  ( mkDMQTracers
  , DMQDiffusionTracers
  , DMQTracers (..)
  , PrometheusConfig
  , DMQStartupTrace (..)
  , Diffusion.NoExtraPeers (..)
  , Diffusion.NoExtraState (..)
  , Diffusion.NoExtraDebugState (..)
  , Diffusion.NoExtraFlags (..)
  , Diffusion.NoExtraConfig (..)
  , Diffusion.NoExtraAPI (..)
  , Diffusion.NoExtraChurnArgs (..)
  ) where

import Codec.CBOR.Term (Term)
import "contra-tracer" Control.Tracer

import Data.Aeson
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import System.Directory (doesFileExist)
import System.Metrics qualified as EKG

import Network.Mux.Trace qualified as Mx
import Network.Mux.Tracing ()
import Network.Socket (HostName, PortNumber)
import Network.TypedProtocol.Codec (AnyMessage (..))

import Ouroboros.Network.Tracing ()

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.Diffusion.Topology (NetworkTopology)
import Ouroboros.Network.Driver (TraceSendRecv)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import Ouroboros.Network.Protocol.KeepAlive.Type (KeepAlive)
import Ouroboros.Network.Protocol.KeepAlive.Type qualified as KA
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Type qualified as PS
import Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as STX
import Ouroboros.Network.Snocket (RemoteAddress)
import Ouroboros.Network.TxSubmission.Inbound.V2 (TraceTxLogic)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
           (TraceTxSubmissionInbound, TxSubmissionCounters)
import Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound)

import Cardano.KESAgent.KES.Crypto (Crypto)
import Cardano.Logging (Namespace (..))
import Cardano.Logging qualified as Logging
import Cardano.Logging.Prometheus.TCPServer qualified as Logging
import Cardano.Network.NodeToClient qualified as Cardano.NtC

import DMQ.Configuration
import DMQ.Diffusion.NodeKernel.Types (ValidationCfg)
import DMQ.NodeToClient.LocalMsgNotification qualified as LMN
import DMQ.NodeToClient.LocalMsgSubmission (TraceLocalMsgSubmission)
import DMQ.NodeToClient.LocalStateQueryClient.Types
           (TraceLocalStateQueryClient (..))
import DMQ.NodeToClient.Version as NtC
import DMQ.NodeToNode.Version as NtN
import DMQ.Policy qualified as Policy
import DMQ.Protocol.LocalMsgNotification.Type (LocalMsgNotification)
import DMQ.Protocol.LocalMsgNotification.Type qualified as LMN
import DMQ.Protocol.LocalMsgSubmission.Type (LocalMsgSubmission,
           LocalTxSubmission)
import DMQ.Protocol.LocalMsgSubmission.Type qualified as LMS
import DMQ.Protocol.SigSubmission.Type (Sig, SigId, SigSubmission,
           SigValidationTrace, Verbose (..))
import DMQ.Protocol.SigSubmissionV2.Type (SigSubmissionV2)
import DMQ.Protocol.SigSubmissionV2.Type qualified as SigSubV2
import DMQ.SigSubmissionV2.Outbound qualified as SigSubV2


data DMQTracers crypto ntnAddr ntcAddr m = DMQTracers {
    sigSubmissionLogicTracer
      :: Tracer m (TraceTxLogic ntnAddr SigId (Sig crypto)),
    sigCountersTracer
      :: Tracer m TxSubmissionCounters,
    sigSubmissionLogicPeerTracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) (TraceTxLogic ntnAddr SigId (Sig crypto))),
    localMsgSubmissionProtocolTracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntcAddr) (TraceSendRecv (LocalMsgSubmission (Sig crypto)))),
    localMsgSubmissionServerTracer
      ::  Tracer m (Mx.WithBearer (ConnectionId ntcAddr) (TraceLocalMsgSubmission SigId)),
    localMsgNotificationProtocolTracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntcAddr) (TraceSendRecv (LocalMsgNotification (Sig crypto)))),
    localMsgNotificationServerTracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntcAddr) (LMN.TraceMessageNotificationServer SigId)),
    sigSubmissionV2ProtocolTracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) (TraceSendRecv (SigSubmissionV2 SigId (Sig crypto)))),
    sigSubmissionInboundTracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) (TraceTxSubmissionInbound SigId (Sig crypto))),
    sigSubmissionV1ProtocolTracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) (TraceSendRecv (SigSubmission crypto))),
    sigSubmissionOutboundV1Tracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) (TraceTxSubmissionOutbound SigId (Sig crypto))),
    sigSubmissionOutboundV2Tracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) (SigSubV2.TraceSigSubmissionOutbound SigId (Sig crypto))),
    keepAliveProtocolTracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) (TraceSendRecv KeepAlive)),
    peerSharingProtocolTracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) (TraceSendRecv (PeerSharing ntnAddr))),

    dmqStartupTracer
      :: Tracer m DMQStartupTrace,
    localStateQueryClientTracer
      :: Tracer m TraceLocalStateQueryClient,
    sigValidationTracer
      :: Tracer m SigValidationTrace,
    localSigValidationTracer
      :: Tracer m SigValidationTrace,
    cardanoNodeHandshakeTracer
      :: Tracer m (NtC.HandshakeTr LocalAddress),
    cardanoNodeHandshakeProtocolTracer
      :: Tracer m (Mx.WithBearer (ConnectionId LocalAddress)
                                 (TraceSendRecv (Handshake Cardano.NtC.NodeToClientVersion Term))),
    cardanoNodeMuxTracer
      :: Tracer m (Mx.WithBearer (ConnectionId LocalAddress) Mx.Trace),
    cardanoNodeChannelTracer
      :: Tracer m (Mx.WithBearer (ConnectionId LocalAddress) Mx.ChannelTrace),
    cardanoNodeBearerTracer
      :: Tracer m (Mx.WithBearer (ConnectionId LocalAddress) Mx.BearerTrace)
    }

data DMQStartupTrace
  = DMQConfiguration Configuration
  | DMQConfigurationError Text
  | DMQCardanoNodeSocketError FilePath
  | DMQTopology (NetworkTopology NoExtraConfig Diffusion.NoExtraFlags)
  | DMQTopologyError Text
  | DMQPrometheus Logging.TracePrometheusSimple
  | DMQValidationCfgWarning NetworkMagic ValidationCfg


instance Logging.LogFormatting DMQStartupTrace where
  forMachine _dtal (DMQConfiguration config) =
    mconcat [ "kind" .= String "Configuration"
            , "config" .= config
            ]
  forMachine _dtal (DMQConfigurationError e) =
    mconcat [ "kind" .= String "ConfigurationError"
            , "error" .= e
            ]
  forMachine _dtal (DMQCardanoNodeSocketError filePath) =
    mconcat [ "kind" .= String "ConfigurationError"
            , "msg" .= String "cardano node socket does not exist"
            , "filePath" .= filePath
            ]
  forMachine _dtal (DMQTopology topology) =
    mconcat [ "kind" .= String "Topology"
            , "topology" .= topology
            ]
  forMachine _dtal (DMQTopologyError e) =
    mconcat [ "kind" .= String "TopologyError"
            , "error" .= e
            ]
  forMachine dtal (DMQPrometheus msg) = Logging.forMachine dtal msg
  forMachine _dtal (DMQValidationCfgWarning magic cfg) =
    mconcat [ "kind" .= String "NonStandardValidationCfg"
            , "networkMagic" .= unNetworkMagic magic
            , "cfg"  .= cfg
            ]

instance Logging.MetaTrace DMQStartupTrace where
  namespaceFor DMQConfiguration{}          = Logging.Namespace [] ["Configuration"]
  namespaceFor DMQConfigurationError{}     = Logging.Namespace [] ["Configuration", "Error"]
  namespaceFor DMQCardanoNodeSocketError{} = Logging.Namespace [] ["Configuration", "Error"]
  namespaceFor DMQTopology{}               = Logging.Namespace [] ["Topology"]
  namespaceFor DMQTopologyError{}          = Logging.Namespace [] ["Topology", "Error"]
  namespaceFor DMQPrometheus {}            = Logging.Namespace [] ["Prometheus"]
  namespaceFor DMQValidationCfgWarning {}  = Logging.Namespace [] ["ValidationCfg"]
  severityFor _ (Just DMQConfigurationError{})     = Just Logging.Critical
  severityFor _ (Just DMQCardanoNodeSocketError{}) = Just Logging.Critical
  severityFor _ (Just DMQTopologyError{})          = Just Logging.Critical
  severityFor _ (Just (DMQValidationCfgWarning magic _))
                                                   | magic == Policy.dmqMainnetNetworkMagic
                                                   = Just Logging.Critical
                                                   | otherwise
                                                   = Just Logging.Warning
  severityFor _ _                                  = Just Logging.Info
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["Configuration"]
    , Logging.Namespace [] ["Configuration", "Error"]
    , Logging.Namespace [] ["Topology"]
    , Logging.Namespace [] ["Topology", "Error"]
    , Logging.Namespace [] ["Prometheus"]
    ]

type DMQDiffusionTracers m =
    Diffusion.Tracers
      RemoteAddress
      NodeToNodeVersion
      NodeToNodeVersionData
      LocalAddress
      NodeToClientVersion
      NodeToClientVersionData
      Diffusion.NoExtraState
      Diffusion.NoExtraDebugState
      Diffusion.NoExtraFlags
      (Diffusion.NoExtraPeers RemoteAddress)
      m

type PrometheusConfig = Maybe (Bool, Maybe HostName, PortNumber)


-- | Default metrics prefix, it is transformed into `dmq_node_` in `Prometheus`.
--
metricsPrefix :: Text
metricsPrefix = "dmq_node."


-- | Create and configure `DMQTracers` and `DMQDiffusionTracers`.
--
mkDMQTracers
  :: ( Show ntnAddr
     , Show ntcAddr
     , ToJSON ntnAddr
     , Typeable ntnAddr
     , Logging.LogFormatting ntnAddr
     , Logging.LogFormatting ntcAddr
     , Crypto crypto
     , Typeable crypto
     )
  => EKG.Store
  -> FilePath
  -> IO ( DMQTracers crypto ntnAddr ntcAddr IO
        , DMQDiffusionTracers IO
        , PrometheusConfig
        )
mkDMQTracers ekgStore dmqConfigFilePath = do
  exist <- doesFileExist dmqConfigFilePath

  traceConfig <-
    if exist
    then
      Logging.readConfigurationWithFallbackAndDefault
        Logging.Info
        Logging.DNormal
        (Logging.Stdout Logging.MachineFormat)
        (Logging.FromFile dmqConfigFilePath)
        Logging.emptyTraceConfig
        { Logging.tcMetricsPrefix = Just metricsPrefix }
    else
      return
        (Logging.mkConfigurationWithFallback
          Logging.Info
          Logging.DNormal
          (Logging.Stdout Logging.MachineFormat)
        ) { Logging.tcMetricsPrefix = Just metricsPrefix }

  ekgTrace <- Logging.ekgTracer traceConfig ekgStore

  configReflection <- Logging.emptyConfigReflection
  stdoutTrace <- Logging.standardTracer
  let trForward = mempty
      mbTrEkg = Just ekgTrace

  !dtMuxTracer <- mkLoggingTracer traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Remote"]

  !dtChannelTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Remote", "Channel"]

  !dtBearerTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Remote", "Bearer"]

  !dtHandshakeTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Handshake", "Remote"]

  !dtLocalMuxTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Local"]

  !dtLocalChannelTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Local", "Channel"]

  !dtLocalBearerTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Local", "Bearer"]

  !dtLocalHandshakeTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Handshake", "Local"]

  !dtDiffusionTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Startup", "DiffusionInit"]

  !dtTraceLocalRootPeersTracer  <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Peers", "LocalRoot"]

  !dtTracePublicRootPeersTracer  <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Peers", "PublicRoot"]

  !dtTraceLedgerPeersTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Peers", "Ledger"]

  !dtTracePeerSelectionTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection", "Selection"]

  !dtDebugPeerSelectionTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection"]

  !dtTracePeerSelectionCounters <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection"]

  !dtPeerSelectionActionsTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection", "Actions"]

  !dtConnectionManagerTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "ConnectionManager", "Remote"]

  !dtConnectionManagerTransitionTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "ConnectionManager", "Transition"]

  !dtServerTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Server", "Local"]

  !dtInboundGovernorTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "InboundGovernor", "Remote"]

  !dtInboundGovernorTransitionTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "InboundGovernor", "Transition"]

  !dtLocalConnectionManagerTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward Nothing -- never conflate metrics of the same name with those originating from `connectionManagerTr`
    ["Net", "ConnectionManager", "Local"]

  !dtLocalServerTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Server", "Local"]

  !dtLocalInboundGovernorTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "InboundGovernor", "Local"]

  !dtDnsTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "DNS"]

  let dmqDifussionTracers = Diffusion.Tracers {
        Diffusion.dtMuxTracer                         = mkTracer $ Logging.traceWith dtMuxTracer,
        Diffusion.dtChannelTracer                     = mkTracer $ Logging.traceWith dtChannelTracer,
        Diffusion.dtBearerTracer                      = mkTracer $ Logging.traceWith dtBearerTracer,
        Diffusion.dtHandshakeTracer                   = mkTracer $ Logging.traceWith dtHandshakeTracer,
        Diffusion.dtLocalMuxTracer                    = mkTracer $ Logging.traceWith dtLocalMuxTracer,
        Diffusion.dtLocalChannelTracer                = mkTracer $ Logging.traceWith dtLocalChannelTracer,
        Diffusion.dtLocalBearerTracer                 = mkTracer $ Logging.traceWith dtLocalBearerTracer,
        Diffusion.dtLocalHandshakeTracer              = mkTracer $ Logging.traceWith dtLocalHandshakeTracer,
        Diffusion.dtDiffusionTracer                   = mkTracer $ Logging.traceWith dtDiffusionTracer,
        Diffusion.dtTraceLocalRootPeersTracer         = mkTracer $ Logging.traceWith dtTraceLocalRootPeersTracer,
        Diffusion.dtTracePublicRootPeersTracer        = mkTracer $ Logging.traceWith dtTracePublicRootPeersTracer,
        Diffusion.dtTraceLedgerPeersTracer            = mkTracer $ Logging.traceWith dtTraceLedgerPeersTracer,
        Diffusion.dtTracePeerSelectionTracer          = mkTracer $ Logging.traceWith dtTracePeerSelectionTracer,
        Diffusion.dtDebugPeerSelectionTracer          = mkTracer $ Logging.traceWith dtDebugPeerSelectionTracer,
        Diffusion.dtTracePeerSelectionCounters        = mkTracer $ Logging.traceWith dtTracePeerSelectionCounters,
        Diffusion.dtPeerSelectionActionsTracer        = mkTracer $ Logging.traceWith dtPeerSelectionActionsTracer,
        Diffusion.dtConnectionManagerTracer           = mkTracer $ Logging.traceWith dtConnectionManagerTracer,
        Diffusion.dtConnectionManagerTransitionTracer = mkTracer $ Logging.traceWith dtConnectionManagerTransitionTracer,
        Diffusion.dtServerTracer                      = mkTracer $ Logging.traceWith dtServerTracer,
        Diffusion.dtInboundGovernorTracer             = mkTracer $ Logging.traceWith dtInboundGovernorTracer,
        Diffusion.dtInboundGovernorTransitionTracer   = mkTracer $ Logging.traceWith dtInboundGovernorTransitionTracer,
        Diffusion.dtLocalConnectionManagerTracer      = mkTracer $ Logging.traceWith dtLocalConnectionManagerTracer,
        Diffusion.dtLocalServerTracer                 = mkTracer $ Logging.traceWith dtLocalServerTracer,
        Diffusion.dtLocalInboundGovernorTracer        = mkTracer $ Logging.traceWith dtLocalInboundGovernorTracer,
        Diffusion.dtDnsTracer                         = mkTracer $ Logging.traceWith dtDnsTracer
      }

  !sigSubmissionLogicTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "Logic"]

  !sigCountersTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "Counters"]

  !sigSubmissionLogicPeerTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "Logic"]

  !localMsgSubmissionProtocolTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "SigSubmission", "Protocol"]

  !localMsgSubmissionServerTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "SigSubmission", "Server"]

  !localMsgNotificationProtocolTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "MsgNotification", "Protocol"]

  !localMsgNotificationServerTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "MsgNotification", "Server"]

  !sigSubmissionV2ProtocolTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "V2", "Protocol"]

  !sigSubmissionInboundTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "Inbound"]

  !sigSubmissionV1ProtocolTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "V1", "Protocol"]

  !sigSubmissionOutboundV1Tracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "V1", "Outbound"]

  !sigSubmissionOutboundV2Tracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "V2", "Outbound"]

  !keepAliveProtocolTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "KeepAlive", "Protocol"]

  !peerSharingProtocolTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerShare", "Protocol"]

  !dmqStartupTracer' <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Startup"]

  !localStateQueryClientTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "LocalStateQuery"]

  !sigValidationTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Sig", "Validation"]

  !localSigValidationTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "Sig", "Validation"]

  !cardanoNodeHandshakeTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "Cardano", "Handshake"]

  !cardanoNodeHandshakeProtocolTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "Cardano", "Handshake", "Protocol"]

  !cardanoNodeMuxTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "Cardano", "Mux", "Local"]

  !cardanoNodeChannelTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "Cardano", "Mux", "Local", "Channel"]

  !cardanoNodeBearerTracer <- mkLoggingTracer
    traceConfig configReflection
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "Cardano", "Mux", "Local", "Bearer"]

  let dmqTracers = DMQTracers {
        sigSubmissionLogicTracer           = mkTracer $ Logging.traceWith sigSubmissionLogicTracer,
        sigCountersTracer                  = mkTracer $ Logging.traceWith sigCountersTracer,
        sigSubmissionLogicPeerTracer       = mkTracer $ Logging.traceWith sigSubmissionLogicPeerTracer,
        localMsgSubmissionProtocolTracer   = mkTracer $ Logging.traceWith localMsgSubmissionProtocolTracer,
        localMsgSubmissionServerTracer     = mkTracer $ Logging.traceWith localMsgSubmissionServerTracer,
        localMsgNotificationProtocolTracer = mkTracer $ Logging.traceWith localMsgNotificationProtocolTracer,
        localMsgNotificationServerTracer   = mkTracer $ Logging.traceWith localMsgNotificationServerTracer,
        sigSubmissionV2ProtocolTracer      = mkTracer $ Logging.traceWith sigSubmissionV2ProtocolTracer,
        sigSubmissionInboundTracer         = mkTracer $ Logging.traceWith sigSubmissionInboundTracer,
        sigSubmissionV1ProtocolTracer      = mkTracer $ Logging.traceWith sigSubmissionV1ProtocolTracer,
        sigSubmissionOutboundV1Tracer      = mkTracer $ Logging.traceWith sigSubmissionOutboundV1Tracer,
        sigSubmissionOutboundV2Tracer      = mkTracer $ Logging.traceWith sigSubmissionOutboundV2Tracer,
        keepAliveProtocolTracer            = mkTracer $ Logging.traceWith keepAliveProtocolTracer,
        peerSharingProtocolTracer          = mkTracer $ Logging.traceWith peerSharingProtocolTracer,
        dmqStartupTracer                   = mkTracer $ Logging.traceWith dmqStartupTracer',
        localStateQueryClientTracer        = mkTracer $ Logging.traceWith localStateQueryClientTracer,
        sigValidationTracer                = mkTracer $ Logging.traceWith sigValidationTracer,
        localSigValidationTracer           = mkTracer $ Logging.traceWith localSigValidationTracer,
        cardanoNodeHandshakeTracer         = mkTracer $ Logging.traceWith cardanoNodeHandshakeTracer,
        cardanoNodeHandshakeProtocolTracer = mkTracer $ Logging.traceWith cardanoNodeHandshakeProtocolTracer,
        cardanoNodeMuxTracer               = mkTracer $ Logging.traceWith cardanoNodeMuxTracer,
        cardanoNodeChannelTracer           = mkTracer $ Logging.traceWith cardanoNodeChannelTracer,
        cardanoNodeBearerTracer            = mkTracer $ Logging.traceWith cardanoNodeBearerTracer
      }

  -- This backend can only be used globally, i.e. will always apply to the namespace root.
  -- Multiple definitions, especially with differing ports, are considered a *misconfiguration*.
  let prometheusConfig :: PrometheusConfig
      prometheusConfig =
        listToMaybe [ (noSuff, mHost, portNo)
                    | options <- Map.elems (Logging.tcOptions traceConfig)
                    , Logging.ConfBackend backends <- options
                    , Logging.PrometheusSimple noSuff mHost portNo <- backends
                    ]

  return (dmqTracers, dmqDifussionTracers, prometheusConfig)


-- | Create and configure a tracer.
--
mkLoggingTracer :: Logging.LogFormatting a
         => Logging.MetaTrace a
         => Logging.TraceConfig
         -> Logging.ConfigReflection
         -> Logging.Trace IO Logging.FormattedMessage
         -> Logging.Trace IO Logging.FormattedMessage
         -> Maybe (Logging.Trace IO Logging.FormattedMessage)
         -> [Text]
         -> IO (Logging.Trace IO a)
mkLoggingTracer traceConfig configReflection stdoutTrace trForward mbTrEkg as = do
  tracer <- Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg as
  Logging.configureTracers configReflection traceConfig [tracer]
  return tracer


--------------------------------------------------------------------------------
-- LocalTxSubmission Tracer
--------------------------------------------------------------------------------

-- TODO: move instances from `Cardano.Node.Tracing.Tracers.NodeToClient` to `ouroboros-network`.

instance Logging.LogFormatting (AnyMessage (LocalTxSubmission tx err)) where
  forMachine _dtal (AnyMessageAndAgency stok LMS.MsgSubmitTx{}) =
    mconcat [ "kind" .= String "MsgSubmitTx"
             , "agency" .= String (Text.pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LMS.MsgAcceptTx{}) =
    mconcat [ "kind" .= String "MsgAcceptTx"
             , "agency" .= String (Text.pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LMS.MsgRejectTx{}) =
    mconcat [ "kind" .= String "MsgRejectTx"
             , "agency" .= String (Text.pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LMS.MsgDone{}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (Text.pack $ show stok)
             ]

instance Logging.MetaTrace (AnyMessage (LMS.LocalTxSubmission tx err)) where
    namespaceFor (AnyMessageAndAgency _agency LMS.MsgSubmitTx{}) =
      Logging.Namespace [] ["SubmitTx"]
    namespaceFor (AnyMessageAndAgency _agency LMS.MsgAcceptTx{}) =
      Logging.Namespace [] ["AcceptTx"]
    namespaceFor (AnyMessageAndAgency _agency LMS.MsgRejectTx{}) =
      Logging.Namespace [] ["RejectTx"]
    namespaceFor (AnyMessageAndAgency _agency LMS.MsgDone{}) =
      Logging.Namespace [] ["Done"]

    severityFor (Logging.Namespace _ ["SubmitTx"]) _ = Just Logging.Debug
    severityFor (Logging.Namespace _ ["AcceptTx"]) _ = Just Logging.Debug
    severityFor (Logging.Namespace _ ["RejectTx"]) _ = Just Logging.Debug
    severityFor (Logging.Namespace _ ["Done"]) _     = Just Logging.Debug
    severityFor _ _                                  = Nothing

    documentFor (Logging.Namespace _ ["SubmitTx"]) = Just
      "The client submits a single transaction and waits a reply."
    documentFor (Logging.Namespace _ ["AcceptTx"]) = Just
      "The server can reply to inform the client that it has accepted the \
        \transaction."
    documentFor (Logging.Namespace _ ["RejectTx"]) = Just
      "The server can reply to inform the client that it has rejected the \
        \transaction. A reason for the rejection is included."
    documentFor (Logging.Namespace _ ["Done"]) = Just
      "The client can terminate the protocol."
    documentFor _ = Nothing

    allNamespaces = [
        Logging.Namespace [] ["SubmitTx"]
      , Logging.Namespace [] ["AcceptTx"]
      , Logging.Namespace [] ["RejectTx"]
      , Logging.Namespace [] ["Done"]
      ]


--------------------------------------------------------------------------------
-- LocalMsgNotification Tracer
--------------------------------------------------------------------------------


instance Crypto crypto => Logging.LogFormatting (AnyMessage (LocalMsgNotification (Sig crypto))) where
  forMachine dtal (AnyMessageAndAgency _agency msg) = case msg of
    LMN.MsgRequest blockingStyle ->
      mconcat [ "type" .= String "MsgRequest"
              , "blockingStyle" .= show blockingStyle
              ]
    LMN.MsgReply msgs hasMore ->
      case dtal of
        Logging.DMaximum ->
          mconcat [ "type" .= String "MsgReply"
                  , "msgs" .= (Verbose <$> Foldable.toList msgs)
                  , "hasMore" .= case hasMore of
                       LMN.HasMore         -> True
                       LMN.DoesNotHaveMore -> False
                  ]
        _ ->
          mconcat [ "type" .= String "MsgReply"
                  , "msgs" .= Foldable.toList msgs
                  , "hasMore" .= case hasMore of
                       LMN.HasMore         -> True
                       LMN.DoesNotHaveMore -> False
                  ]
    LMN.MsgClientDone ->
      mconcat [ "type" .= String "MsgClientDone"
              ]
instance Logging.MetaTrace (AnyMessage (LocalMsgNotification (Sig crypto))) where
  namespaceFor (AnyMessage msg) = case msg of
    LMN.MsgRequest {}    -> Logging.Namespace [] ["MsgRequest"]
    LMN.MsgReply {}      -> Logging.Namespace [] ["MsgReply"]
    LMN.MsgClientDone {} -> Logging.Namespace [] ["MsgClientDone"]

  severityFor _ _ = Just Logging.Debug
  documentFor _ = Nothing
  allNamespaces = [
      Logging.Namespace [] ["MsgRequest"]
    , Logging.Namespace [] ["MsgReply"]
    , Logging.Namespace [] ["MsgClientDone"]
    ]


--------------------------------------------------------------------------------
-- TraceMessageNotificationServer Tracer
--------------------------------------------------------------------------------


instance Logging.LogFormatting (LMN.TraceMessageNotificationServer SigId) where
  forMachine _ (LMN.TraceMsgNotificationServerReply hasMore msgids) =
    mconcat
      [ "type" .= String "Reply"
      , "hasMore" .= case hasMore of
           LMN.HasMore         -> True
           LMN.DoesNotHaveMore -> False
      , "msgids" .= msgids
      ]
  forMachine _ LMN.TraceMsgNotificationServerHandleDone =
    mconcat
      [ "type" .= String "Done" ]
instance Logging.MetaTrace (LMN.TraceMessageNotificationServer SigId) where
  namespaceFor LMN.TraceMsgNotificationServerReply {}   = Logging.Namespace [] ["TraceMsgNotificationServerReply"]
  namespaceFor LMN.TraceMsgNotificationServerHandleDone = Logging.Namespace [] ["TraceMsgNotificationServerHandleDone"]

  severityFor _ _ = Just Logging.Debug
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["TraceMsgNotificationServerReply"]
    , Logging.Namespace [] ["TraceMsgNotificationServerHandleDone"]
    ]


--------------------------------------------------------------------------------
-- SigSubmissionV2 Tracer
--------------------------------------------------------------------------------

instance Crypto crypto
      => Logging.LogFormatting (AnyMessage (SigSubmissionV2 SigId (Sig crypto))) where
  forMachine _dtal (AnyMessageAndAgency stok SigSubV2.MsgRequestSigIds {}) =
    mconcat
      [ "kind" .= String "MsgRequestSigIds"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (SigSubV2.MsgReplySigIds sigs)) =
    mconcat
      [ "kind" .= String "MsgReplySigIds"
      , "agency" .= String (Text.pack $ show stok)
      , "sigds" .= Foldable.toList sigs
      ]
  forMachine _dtal (AnyMessageAndAgency stok SigSubV2.MsgReplyNoSigIds) =
    mconcat
      [ "kind" .= String "MsgReplyNoSigIds"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (SigSubV2.MsgRequestSigs sigids)) =
    mconcat
      [ "kind" .= String "MsgRequestSigs"
      , "agency" .= String (Text.pack $ show stok)
      , "sigids" .= sigids
      ]
  forMachine dtal (AnyMessageAndAgency stok (SigSubV2.MsgReplySigs sigs)) =
    case dtal of
      Logging.DMaximum ->
        mconcat
          [ "kind" .= String "MsgReplyTxs"
          , "agency" .= String (Text.pack $ show stok)
          , "sigs" .= (Verbose <$> sigs)
          ]
      _ ->
        mconcat
          [ "kind" .= String "MsgReplyTxs"
          , "agency" .= String (Text.pack $ show stok)
          , "sigs" .= sigs
          ]
  forMachine _dtal (AnyMessageAndAgency stok SigSubV2.MsgDone) =
    mconcat
      [ "kind" .= String "MsgDone"
      , "agency" .= String (Text.pack $ show stok)
      ]
instance Logging.MetaTrace (AnyMessage (SigSubmissionV2 SigId (Sig crypto))) where
  namespaceFor (AnyMessageAndAgency _stok SigSubV2.MsgRequestSigIds {}) =
    Namespace [] ["RequestSigIds"]
  namespaceFor (AnyMessageAndAgency _stok SigSubV2.MsgReplySigIds {}) =
    Namespace [] ["ReplySigIds"]
  namespaceFor (AnyMessageAndAgency _stok SigSubV2.MsgReplyNoSigIds) =
    Namespace [] ["ReplyNoSigIds"]
  namespaceFor (AnyMessageAndAgency _stok SigSubV2.MsgRequestSigs {}) =
    Namespace [] ["RequestSigs"]
  namespaceFor (AnyMessageAndAgency _stok SigSubV2.MsgReplySigs {}) =
    Namespace [] ["ReplySigs"]
  namespaceFor (AnyMessageAndAgency _stok SigSubV2.MsgDone {}) =
    Namespace [] ["Done"]

  severityFor _ (Just _) = Just Logging.Debug
  severityFor _ Nothing  = Nothing

  documentFor _ = Nothing

  allNamespaces = [
      Namespace [] ["MsgInit"]
    , Namespace [] ["RequestSigIds"]
    , Namespace [] ["ReplySigIds"]
    , Namespace [] ["ReplyNoSigIds"]
    , Namespace [] ["RequestSigs"]
    , Namespace [] ["ReplySigs"]
    , Namespace [] ["Done"]
    ]

--------------------------------------------------------------------------------
-- SigSubmissionV1 Tracer
--------------------------------------------------------------------------------

-- TODO: move instances from `Cardano.Node.Tracing.Tracers.NodeToNode` to `ouroboros-network`.

instance (ToJSON txid, ToJSON tx, ToJSON (Verbose tx))
      => Logging.LogFormatting (AnyMessage (TxSubmission2 txid tx)) where
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgInit) =
    mconcat
      [ "kind" .= String "MsgInit"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgRequestTxIds _ numToAck numToReq)) =
    mconcat
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (Text.pack $ show stok)
      , "numToAck" .= STX.getNumTxIdsToAck numToAck
      , "numToReq" .= STX.getNumTxIdsToReq numToReq
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxIds txids)) =
    mconcat
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (Text.pack $ show stok)
      , "txIds" .= Foldable.toList txids
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgRequestTxs txids)) =
    mconcat
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (Text.pack $ show stok)
      , "txIds" .= txids
      ]
  forMachine dtal (AnyMessageAndAgency stok (STX.MsgReplyTxs txs)) =
    case dtal of
      Logging.DMaximum ->
        mconcat
          [ "kind" .= String "MsgReplyTxs"
          , "agency" .= String (Text.pack $ show stok)
          , "txs" .= (Verbose <$> txs)
          ]
      _ ->
        mconcat
          [ "kind" .= String "MsgReplyTxs"
          , "agency" .= String (Text.pack $ show stok)
          , "txs" .= txs
          ]
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgDone) =
    mconcat
      [ "kind" .= String "MsgDone"
      , "agency" .= String (Text.pack $ show stok)
      ]

instance Logging.MetaTrace (AnyMessage (TxSubmission2 SigId (Sig crypto))) where
    namespaceFor (AnyMessageAndAgency _stok STX.MsgInit {}) =
      Namespace [] ["MsgInit"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgRequestTxIds {}) =
      Namespace [] ["RequestTxIds"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgReplyTxIds {}) =
      Namespace [] ["ReplyTxIds"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgRequestTxs {}) =
      Namespace [] ["RequestTxs"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgReplyTxs {}) =
      Namespace [] ["ReplyTxs"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgDone {}) =
      Namespace [] ["Done"]

    severityFor (Namespace _ ["MsgInit"]) _      = Just Logging.Debug
    severityFor (Namespace _ ["RequestTxIds"]) _ = Just Logging.Debug
    severityFor (Namespace _ ["ReplyTxIds"]) _   = Just Logging.Debug
    severityFor (Namespace _ ["RequestTxs"]) _   = Just Logging.Debug
    severityFor (Namespace _ ["ReplyTxs"]) _     = Just Logging.Debug
    severityFor (Namespace _ ["Done"]) _         = Just Logging.Debug
    severityFor _ _                              = Nothing

    documentFor (Namespace _ ["MsgInit"]) = Just
        "Client side hello message."
    documentFor (Namespace _ ["RequestTxIds"]) = Just $ mconcat
      [ "Request a non-empty list of transaction identifiers from the client, "
      , "and confirm a number of outstanding transaction identifiers. "
      , "\n "
      , "With 'TokBlocking' this is a blocking operation: the response will "
      , "always have at least one transaction identifier, and it does not expect "
      , "a prompt response: there is no timeout. This covers the case when there "
      , "is nothing else to do but wait. For example this covers leaf nodes that "
      , "rarely, if ever, create and submit a transaction. "
      , "\n "
      , "With 'TokNonBlocking' this is a non-blocking operation: the response "
      , "may be an empty list and this does expect a prompt response. This "
      , "covers high throughput use cases where we wish to pipeline, by "
      , "interleaving requests for additional transaction identifiers with "
      , "requests for transactions, which requires these requests not block. "
      , "\n "
      , "The request gives the maximum number of transaction identifiers that "
      , "can be accepted in the response. This must be greater than zero in the "
      , "'TokBlocking' case. In the 'TokNonBlocking' case either the numbers "
      , "acknowledged or the number requested must be non-zero. In either case, "
      , "the number requested must not put the total outstanding over the fixed "
      , "protocol limit. "
      , "\n"
      , "The request also gives the number of outstanding transaction "
      , "identifiers that can now be acknowledged. The actual transactions "
      , "to acknowledge are known to the peer based on the FIFO order in which "
      , "they were provided. "
      , "\n "
      , "There is no choice about when to use the blocking case versus the "
      , "non-blocking case, it depends on whether there are any remaining "
      , "unacknowledged transactions (after taking into account the ones "
      , "acknowledged in this message): "
      , "\n "
      , "* The blocking case must be used when there are zero remaining "
      , "  unacknowledged transactions. "
      , "\n "
      , "* The non-blocking case must be used when there are non-zero remaining "
      , "  unacknowledged transactions."
      ]
    documentFor (Namespace _ ["ReplyTxIds"]) = Just $ mconcat
      [ "Reply with a list of transaction identifiers for available "
      , "transactions, along with the size of each transaction. "
      , "\n "
      , "The list must not be longer than the maximum number requested. "
      , "\n "
      , "In the 'StTxIds' 'StBlocking' state the list must be non-empty while "
      , "in the 'StTxIds' 'StNonBlocking' state the list may be empty. "
      , "\n "
      , "These transactions are added to the notional FIFO of outstanding "
      , "transaction identifiers for the protocol. "
      , "\n "
      , "The order in which these transaction identifiers are returned must be "
      , "the order in which they are submitted to the mempool, to preserve "
      , "dependent transactions."
      ]
    documentFor (Namespace _ ["RequestTxs"]) = Just $ mconcat
      [ "Request one or more transactions corresponding to the given  "
      , "transaction identifiers.  "
      , "\n "
      , "While it is the responsibility of the replying peer to keep within "
      , "pipelining in-flight limits, the sender must also cooperate by keeping "
      , "the total requested across all in-flight requests within the limits. "
      , "\n"
      , "It is an error to ask for transaction identifiers that were not "
      , "previously announced (via 'MsgReplyTxIds'). "
      , "\n"
      , "It is an error to ask for transaction identifiers that are not "
      , "outstanding or that were already asked for."
      ]
    documentFor (Namespace _ ["ReplyTxs"]) = Just $ mconcat
      [ "Reply with the requested transactions, or implicitly discard."
      , "\n"
      , "Transactions can become invalid between the time the transaction "
      , "identifier was sent and the transaction being requested. Invalid "
      , "(including committed) transactions do not need to be sent."
      , "\n"
      , "Any transaction identifiers requested but not provided in this reply "
      , "should be considered as if this peer had never announced them. (Note "
      , "that this is no guarantee that the transaction is invalid, it may still "
      , "be valid and available from another peer)."
      ]
    documentFor (Namespace _ ["Done"]) = Just $ mconcat
      [ "Termination message, initiated by the client when the server is "
      , "making a blocking call for more transaction identifiers."
      ]
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["MsgInit"]
      , Namespace [] ["RequestTxIds"]
      , Namespace [] ["ReplyTxIds"]
      , Namespace [] ["RequestTxs"]
      , Namespace [] ["ReplyTxs"]
      , Namespace [] ["Done"]
      ]

instance Crypto crypto
      => Logging.LogFormatting (SigSubV2.TraceSigSubmissionOutbound SigId (Sig crypto)) where
  forMachine _ (SigSubV2.TraceSigSubmissionOutboundRecvMsgRequestSigs sigids) =
    mconcat [ "kind" .= String "TraceSigSubmissionOutboundRecvMsgRequestSigs"
            , "sigids" .= sigids
            ]
  forMachine dtal (SigSubV2.TraceSigSubmissionOutboundSendMsgReplySigs sigs) =
    case dtal of
      Logging.DMaximum ->
        mconcat [ "kind" .= String "TraceSigSubmissionOutboundSendMsgReplySigs"
                , "sigs" .= (Verbose <$> sigs)
                ]
      _ ->
        mconcat [ "kind" .= String "TraceSigSubmissionOutboundSendMsgReplySigs"
                , "sigs" .= sigs
                ]

instance Logging.MetaTrace (SigSubV2.TraceSigSubmissionOutbound SigId (Sig crypto)) where
  namespaceFor SigSubV2.TraceSigSubmissionOutboundRecvMsgRequestSigs {} = Logging.Namespace [] ["TraceSigSubmissionOutboundRecvMsgRequestSigs"]
  namespaceFor SigSubV2.TraceSigSubmissionOutboundSendMsgReplySigs {} = Logging.Namespace [] ["TraceSigSubmissionOutboundSendMsgReplySigs"]
  severityFor _ _ = Just Logging.Debug
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["TraceSigSubmissionOutboundRecvMsgRequestSigs"]
    , Logging.Namespace [] ["TraceSigSubmissionOutboundSendMsgReplySigs"]
    ]



--------------------------------------------------------------------------------
-- KeepAlive Tracer
--------------------------------------------------------------------------------

-- TODO: move instances from `Cardano.Node.Tracing.Tracers.NodeToNode` to `ouroboros-network`.

instance Logging.LogFormatting (AnyMessage KA.KeepAlive) where
  forMachine _dtal (AnyMessageAndAgency stok KA.MsgKeepAlive {}) =
    mconcat
      [ "kind" .= String "KeepAlive"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok KA.MsgKeepAliveResponse {}) =
    mconcat
      [ "kind" .= String "KeepAliveResponse"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok KA.MsgDone) =
    mconcat
      [ "kind" .= String "Done"
      , "agency" .= String (Text.pack $ show stok)
      ]

instance Logging.MetaTrace (AnyMessage KA.KeepAlive) where
    namespaceFor (AnyMessageAndAgency _stok KA.MsgKeepAlive {}) =
      Namespace [] ["KeepAlive"]
    namespaceFor (AnyMessageAndAgency _stok KA.MsgKeepAliveResponse {}) =
      Namespace [] ["KeepAliveResponse"]
    namespaceFor (AnyMessageAndAgency _stok KA.MsgDone) =
      Namespace [] ["Done"]

    severityFor (Namespace _ ["KeepAlive"]) _         = Just Logging.Debug
    severityFor (Namespace _ ["KeepAliveResponse"]) _ = Just Logging.Debug
    severityFor (Namespace _ ["Done"]) _              = Just Logging.Debug
    severityFor _ _                                   = Nothing

    documentFor (Namespace _ ["KeepAlive"]) = Just
        "Client side message to keep the connection alive."
    documentFor (Namespace _ ["KeepAliveResponse"]) = Just $ mconcat
      [ "Server side response to a previous client KeepAlive message."
      ]
    documentFor (Namespace _ ["Done"]) = Just $ mconcat
      [ "Termination message, initiated by the client."
      ]
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["KeepAlive"]
      , Namespace [] ["KeepAliveResponse"]
      , Namespace [] ["Done"]
      ]

--------------------------------------------------------------------------------
-- PeerSharing Tracer
--------------------------------------------------------------------------------

-- TODO: move instances from `Cardano.Node.Tracing.Tracers.NodeToNode` to `ouroboros-network`.

instance ToJSON addr => Logging.LogFormatting (AnyMessage (PS.PeerSharing addr)) where
  forMachine _dtal (AnyMessageAndAgency stok (PS.MsgShareRequest num)) =
    mconcat
      [ "kind" .= String "MsgPeerShareRequest"
      , "agency" .= String (Text.pack $ show stok)
      , "amount" .= PS.getAmount num
      ]
  forMachine _dtal (AnyMessageAndAgency stok (PS.MsgSharePeers peers)) =
    mconcat
      [ "kind" .= String "MsgSharePeers"
      , "agency" .= String (Text.pack $ show stok)
      , "peers" .= peers
      ]
  forMachine _dtal (AnyMessageAndAgency stok PS.MsgDone) =
    mconcat
      [ "kind" .= String "Done"
      , "agency" .= String (Text.pack $ show stok)
      ]

instance Logging.MetaTrace (AnyMessage (PS.PeerSharing addr)) where
    namespaceFor (AnyMessageAndAgency _stok PS.MsgShareRequest {}) =
      Namespace [] ["PeerShareRequest"]
    namespaceFor (AnyMessageAndAgency _stok PS.MsgSharePeers {}) =
      Namespace [] ["PeerShareResult"]
    namespaceFor (AnyMessageAndAgency _stok PS.MsgDone) =
      Namespace [] ["PeerShareDone"]

    severityFor (Namespace _ ["PeerShareRequest"]) _ = Just Logging.Debug
    severityFor (Namespace _ ["PeerShareResult"]) _  = Just Logging.Debug
    severityFor (Namespace _ ["PeerShareDone"]) _    = Just Logging.Debug
    severityFor _ _                                  = Nothing

    documentFor (Namespace _ ["PeerShareRequest"]) = Just "Client asks for peers."
    documentFor (Namespace _ ["PeerShareResult"]) = Just "Server responds with peers."
    documentFor (Namespace _ ["PeerShareDone"]) = Just "Termination message, initiated by the client."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["PeerShareRequest"]
      , Namespace [] ["PeerShareResult"]
      , Namespace [] ["PeerShareDone"]
      ]


instance ToJSON (PublicRootPeers (Diffusion.NoExtraPeers RemoteAddress) RemoteAddress) where
  toJSON prp =
    object [ "kind"              .= String "PublicRootPeers"
           , "ledgerPeers"       .= PublicRootPeers.getLedgerPeers prp
           , "bigLedgerPeers"    .= PublicRootPeers.getBigLedgerPeers prp
           ]
