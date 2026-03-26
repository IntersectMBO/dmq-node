{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DMQ.Tracer
  ( mkCardanoTracer
  , DMQTracers (..)
  , DMQStartupTrace (..)
  , NoExtraPeers (..)
  , NoExtraState (..)
  , NoExtraDebugState (..)
  , NoExtraCounters (..)
  , NoExtraFlags (..)
  , NoExtraConfig (..)
  , NoExtraAPI (..)
  , NoExtraChurnArgs (..)
  , NoExtraTracer (..)
  ) where

import Codec.CBOR.Term (Term)
import "contra-tracer" Control.Tracer

import Data.Aeson
import Data.Foldable qualified as Foldable
import Data.Text qualified as Text
import Data.Typeable (Typeable)

import Network.Mux.Trace qualified as Mx
import Network.Mux.Tracing ()
import Network.TypedProtocol.Codec (AnyMessage (..))

import Ouroboros.Network.Tracing ()
import Ouroboros.Network.Tracing.PeerSelection ()
import Ouroboros.Network.Tracing.TxSubmission ()

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.Diffusion (NoExtraPeers (..))
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.Diffusion.Topology (NetworkTopology)
import Ouroboros.Network.Driver (TraceSendRecv)
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.Protocol.KeepAlive.Type (KeepAlive)
import Ouroboros.Network.Protocol.KeepAlive.Type qualified as KA
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Type qualified as PS
import Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as STX
import Ouroboros.Network.Snocket (RemoteAddress)
import Ouroboros.Network.Tracing.TxSubmission.Inbound ()
import Ouroboros.Network.Tracing.TxSubmission.Outbound ()
import Ouroboros.Network.TxSubmission.Inbound.V2 (TraceTxLogic)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
           (TraceTxSubmissionInbound)
import Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound)

import Cardano.KESAgent.KES.Crypto (Crypto)
import Cardano.Logging (Namespace (..))
import Cardano.Logging qualified as Logging

import DMQ.Configuration
import DMQ.NodeToClient.LocalMsgNotification qualified as LMN
import DMQ.NodeToClient.LocalMsgSubmission (TraceLocalMsgSubmission)
import DMQ.NodeToClient.LocalStateQueryClient.Types
           (TraceLocalStateQueryClient (..))
import DMQ.NodeToClient.Version as NtC
import DMQ.NodeToNode.Version as NtN
import DMQ.Protocol.LocalMsgNotification.Type (LocalMsgNotification)
import DMQ.Protocol.LocalMsgNotification.Type qualified as LMN
import DMQ.Protocol.LocalMsgSubmission.Type (LocalMsgSubmission,
           LocalTxSubmission)
import DMQ.Protocol.LocalMsgSubmission.Type qualified as LMS
import DMQ.Protocol.SigSubmission.Type (Sig, SigId, SigSubmission,
           SigValidationTrace)
import DMQ.Protocol.SigSubmissionV2.Type (SigSubmissionV2)
import DMQ.Protocol.SigSubmissionV2.Type qualified as SigSubV2
import DMQ.SigSubmissionV2.Outbound qualified as SigSubV2


data DMQTracers crypto ntnAddr ntcAddr m = DMQTracers {
    sigSubmissionLogicTracer
      :: Tracer m (TraceTxLogic ntnAddr SigId (Sig crypto)),
    sigSubmissionLogicPeerTracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) (TraceTxLogic ntnAddr SigId (Sig crypto))),
    localMsgSubmissionProtocolTracer
      :: Tracer m (Mx.WithBearer (ConnectionId ntcAddr) (TraceSendRecv (LocalMsgSubmission (Sig crypto)))),
    localMsgSubmissionServerTracer
      ::  Tracer m (Mx.WithBearer (ConnectionId ntcAddr) (TraceLocalMsgSubmission (Sig crypto) SigId)),
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
      :: Tracer m (NtC.HandshakeTr LocalAddress)
    }

data DMQStartupTrace
  = DMQConfiguration Configuration
  | DMQTopology (NetworkTopology NoExtraConfig NoExtraFlags)


instance Logging.LogFormatting DMQStartupTrace where
  forMachine _dtal (DMQConfiguration config) =
    mconcat [ "kind" .= String "Configuration"
            , "config" .= config
            ]
  forMachine _dtal (DMQTopology topology) =
    mconcat [ "kind" .= String "Topology"
            , "topology" .= topology
            ]
instance Logging.MetaTrace DMQStartupTrace where
  namespaceFor DMQConfiguration{} = Logging.Namespace [] ["DMQ", "Configuration"]
  namespaceFor DMQTopology{} = Logging.Namespace [] ["DMQ", "Topology"]
  severityFor _ _ = Just Logging.Info
  documentFor _ = Nothing
  allNamespaces = [ Logging.Namespace [] ["Trace"] ]


mkCardanoTracer :: ( Show ntnAddr
                   , Show ntcAddr
                   , ToJSON ntnAddr
                   , Typeable ntnAddr
                   , Logging.LogFormatting ntnAddr
                   , Logging.LogFormatting ntcAddr
                   , Crypto crypto
                   , Typeable crypto
                   )
                => FilePath
                -> IO ( DMQTracers crypto ntnAddr ntcAddr IO
                      , Diffusion.Tracers
                          RemoteAddress
                          NodeToNodeVersion
                          NodeToNodeVersionData
                          LocalAddress
                          NodeToClientVersion
                          NodeToClientVersionData
                          NoExtraState
                          NoExtraDebugState
                          NoExtraFlags
                          (NoExtraPeers RemoteAddress)
                          IO
                      )
mkCardanoTracer dmqConfigFilePath = do
  traceConfig <- Logging.readConfiguration dmqConfigFilePath
  emptyConfigReflection <- Logging.emptyConfigReflection
  stdoutTrace <- Logging.standardTracer
  let trForward = mempty
  let mbTrEkg = Nothing
  -- Make it a "contra-tracer" tracer for backward compatibility.
  !dtMuxTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Remote"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtMuxTracer]
  !dtChannelTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Remote", "Channel"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtChannelTracer]
  !dtBearerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Remote", "Bearer"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtBearerTracer]
  !dtHandshakeTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Handshake", "Remote"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtHandshakeTracer]
  !dtLocalMuxTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalMuxTracer]
  !dtLocalChannelTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Local", "Channel"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalChannelTracer]
  !dtLocalBearerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Local", "Bearer"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalBearerTracer]
  !dtLocalHandshakeTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Handshake", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalHandshakeTracer]
  !dtDiffusionTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Startup", "DiffusionInit"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtDiffusionTracer]
  !dtTraceLocalRootPeersTracer  <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Peers", "LocalRoot"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtTraceLocalRootPeersTracer]
  !dtTracePublicRootPeersTracer  <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Peers", "PublicRoot"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtTracePublicRootPeersTracer]
  !dtTraceLedgerPeersTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Peers", "Ledger"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtTraceLedgerPeersTracer]
  !dtTracePeerSelectionTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection", "Selection"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtTracePeerSelectionTracer]
  !dtDebugPeerSelectionTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtDebugPeerSelectionTracer]
  !dtTracePeerSelectionCounters <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtTracePeerSelectionCounters]
  !dtPeerSelectionActionsTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection", "Actions"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtPeerSelectionActionsTracer]
  !dtConnectionManagerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "ConnectionManager", "Remote"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtConnectionManagerTracer]
  !dtConnectionManagerTransitionTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "ConnectionManager", "Transition"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtConnectionManagerTransitionTracer]
  !dtServerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Server", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtServerTracer]
  !dtInboundGovernorTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "InboundGovernor", "Remote"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtInboundGovernorTracer]
  !dtInboundGovernorTransitionTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "InboundGovernor", "Transition"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtInboundGovernorTransitionTracer]
  !dtLocalConnectionManagerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward Nothing -- never conflate metrics of the same name with those originating from `connectionManagerTr`
    ["Net", "ConnectionManager", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalConnectionManagerTracer]
  !dtLocalServerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Server", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalServerTracer]
  !dtLocalInboundGovernorTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "InboundGovernor", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalInboundGovernorTracer]
  !dtDnsTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "DNS"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtDnsTracer]
  let dmqDifussionTracers =
        -- From `Cardano.Node.Tracing.Tracers`
        -- Branch "ana/10.6-final-integration-mix"
        Diffusion.Tracers {
            Diffusion.dtMuxTracer                                  = Tracer $ Logging.traceWith dtMuxTracer,
            Diffusion.dtChannelTracer                              = Tracer $ Logging.traceWith dtChannelTracer,
            Diffusion.dtBearerTracer                               = Tracer $ Logging.traceWith dtBearerTracer,
            Diffusion.dtHandshakeTracer                            = Tracer $ Logging.traceWith dtHandshakeTracer,
            Diffusion.dtLocalMuxTracer                             = Tracer $ Logging.traceWith dtLocalMuxTracer,
            Diffusion.dtLocalChannelTracer                         = Tracer $ Logging.traceWith dtLocalChannelTracer,
            Diffusion.dtLocalBearerTracer                          = Tracer $ Logging.traceWith dtLocalBearerTracer,
            Diffusion.dtLocalHandshakeTracer                       = Tracer $ Logging.traceWith dtLocalHandshakeTracer,
            Diffusion.dtDiffusionTracer                            = Tracer $ Logging.traceWith dtDiffusionTracer,
            Diffusion.dtTraceLocalRootPeersTracer                  = Tracer $ Logging.traceWith dtTraceLocalRootPeersTracer,
            Diffusion.dtTracePublicRootPeersTracer                 = Tracer $ Logging.traceWith dtTracePublicRootPeersTracer,
            Diffusion.dtTraceLedgerPeersTracer                     = Tracer $ Logging.traceWith dtTraceLedgerPeersTracer,
            Diffusion.dtTracePeerSelectionTracer                   = Tracer $ Logging.traceWith dtTracePeerSelectionTracer,
            Diffusion.dtDebugPeerSelectionTracer                   = Tracer $ Logging.traceWith dtDebugPeerSelectionTracer,
            Diffusion.dtTracePeerSelectionCounters                 = Tracer $ Logging.traceWith dtTracePeerSelectionCounters,
            Diffusion.dtPeerSelectionActionsTracer                 = Tracer $ Logging.traceWith dtPeerSelectionActionsTracer,
            Diffusion.dtConnectionManagerTracer                    = Tracer $ Logging.traceWith dtConnectionManagerTracer,
            Diffusion.dtConnectionManagerTransitionTracer          = Tracer $ Logging.traceWith dtConnectionManagerTransitionTracer,
            Diffusion.dtServerTracer                               = Tracer $ Logging.traceWith dtServerTracer,
            Diffusion.dtInboundGovernorTracer                      = Tracer $ Logging.traceWith dtInboundGovernorTracer,
            Diffusion.dtInboundGovernorTransitionTracer            = Tracer $ Logging.traceWith dtInboundGovernorTransitionTracer,
            Diffusion.dtLocalConnectionManagerTracer               = Tracer $ Logging.traceWith dtLocalConnectionManagerTracer,
            Diffusion.dtLocalServerTracer                          = Tracer $ Logging.traceWith dtLocalServerTracer,
            Diffusion.dtLocalInboundGovernorTracer                 = Tracer $ Logging.traceWith dtLocalInboundGovernorTracer,
            Diffusion.dtDnsTracer                                  = Tracer $ Logging.traceWith dtDnsTracer
          }

  !sigSubmissionLogicTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "Logic"]
  Logging.configureTracers emptyConfigReflection traceConfig [sigSubmissionLogicTracer]

  !sigSubmissionLogicPeerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "Logic"]
  Logging.configureTracers emptyConfigReflection traceConfig [sigSubmissionLogicPeerTracer]

  !localMsgSubmissionProtocolTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "SigSubmission", "Protocol"]
  Logging.configureTracers emptyConfigReflection traceConfig [localMsgSubmissionProtocolTracer]

  !localMsgSubmissionServerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "SigSubmission", "Server"]
  Logging.configureTracers emptyConfigReflection traceConfig [localMsgSubmissionServerTracer]

  !localMsgNotificationProtocolTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "MsgNotification", "Protocol"]
  Logging.configureTracers emptyConfigReflection traceConfig [localMsgNotificationProtocolTracer]

  !localMsgNotificationServerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "MsgNotification", "Server"]
  Logging.configureTracers emptyConfigReflection traceConfig [localMsgNotificationServerTracer]

  !sigSubmissionV2ProtocolTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "V2", "Protocol"]
  Logging.configureTracers emptyConfigReflection traceConfig [sigSubmissionV2ProtocolTracer]

  !sigSubmissionInboundTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "Inbound"]
  Logging.configureTracers emptyConfigReflection traceConfig [sigSubmissionInboundTracer]

  !sigSubmissionV1ProtocolTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "V1", "Protocol"]
  Logging.configureTracers emptyConfigReflection traceConfig [sigSubmissionV1ProtocolTracer]

  !sigSubmissionOutboundV1Tracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "V1", "Outbound"]
  Logging.configureTracers emptyConfigReflection traceConfig [sigSubmissionOutboundV1Tracer]

  !sigSubmissionOutboundV2Tracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "SigSubmission", "V2", "Outbound"]
  Logging.configureTracers emptyConfigReflection traceConfig [sigSubmissionOutboundV2Tracer]

  !keepAliveProtocolTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "KeepAlive", "Protocol"]
  Logging.configureTracers emptyConfigReflection traceConfig [keepAliveProtocolTracer]

  !peerSharingProtocolTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerShare", "Protocol"]
  Logging.configureTracers emptyConfigReflection traceConfig [peerSharingProtocolTracer]

  !dmqStartupTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Startup"]
  Logging.configureTracers emptyConfigReflection traceConfig [dmqStartupTracer]

  !localStateQueryClientTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "LocalStateQuery"]
  Logging.configureTracers emptyConfigReflection traceConfig [localStateQueryClientTracer]

  !sigValidationTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Sig", "Validation"]
  Logging.configureTracers emptyConfigReflection traceConfig [sigValidationTracer]

  !localSigValidationTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "Sig", "Validation"]
  Logging.configureTracers emptyConfigReflection traceConfig [localSigValidationTracer]

  !cardanoNodeHandshakeTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Local", "Cardano", "Handshake"]
  Logging.configureTracers emptyConfigReflection traceConfig [cardanoNodeHandshakeTracer]

  let dmqTracers = DMQTracers {
        sigSubmissionLogicTracer           = Tracer $ Logging.traceWith sigSubmissionLogicTracer,
        sigSubmissionLogicPeerTracer       = Tracer $ Logging.traceWith sigSubmissionLogicPeerTracer,
        localMsgSubmissionProtocolTracer   = Tracer $ Logging.traceWith localMsgSubmissionProtocolTracer,
        localMsgSubmissionServerTracer     = Tracer $ Logging.traceWith localMsgSubmissionServerTracer,
        localMsgNotificationProtocolTracer = Tracer $ Logging.traceWith localMsgNotificationProtocolTracer,
        localMsgNotificationServerTracer   = Tracer $ Logging.traceWith localMsgNotificationServerTracer,
        sigSubmissionV2ProtocolTracer      = Tracer $ Logging.traceWith sigSubmissionV2ProtocolTracer,
        sigSubmissionInboundTracer         = Tracer $ Logging.traceWith sigSubmissionInboundTracer,
        sigSubmissionV1ProtocolTracer      = Tracer $ Logging.traceWith sigSubmissionV1ProtocolTracer,
        sigSubmissionOutboundV1Tracer      = Tracer $ Logging.traceWith sigSubmissionOutboundV1Tracer,
        sigSubmissionOutboundV2Tracer      = Tracer $ Logging.traceWith sigSubmissionOutboundV2Tracer,
        keepAliveProtocolTracer            = Tracer $ Logging.traceWith keepAliveProtocolTracer,
        peerSharingProtocolTracer          = Tracer $ Logging.traceWith peerSharingProtocolTracer,
        dmqStartupTracer                   = Tracer $ Logging.traceWith dmqStartupTracer,
        localStateQueryClientTracer        = Tracer $ Logging.traceWith localStateQueryClientTracer,
        sigValidationTracer                = Tracer $ Logging.traceWith sigValidationTracer,
        localSigValidationTracer           = Tracer $ Logging.traceWith localSigValidationTracer,
        cardanoNodeHandshakeTracer         = Tracer $ Logging.traceWith cardanoNodeHandshakeTracer
      }

  return (dmqTracers, dmqDifussionTracers)

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
  forMachine _dtal (AnyMessageAndAgency _agency msg) = case msg of
    LMN.MsgRequest blockingStyle ->
      mconcat [ "type" .= String "MsgRequest"
              , "blockingStyle" .= show blockingStyle
              ]
    LMN.MsgReply msgs hasMore ->
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

instance Logging.LogFormatting (AnyMessage (SigSubmissionV2 SigId (Sig crypto))) where
  forMachine _dtal (AnyMessageAndAgency stok SigSubV2.MsgRequestSigIds {}) =
    mconcat
      [ "kind" .= String "MsgRequestSigIds"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (SigSubV2.MsgReplySigIds _)) =
    mconcat
      [ "kind" .= String "MsgReplySigIds"
      , "agency" .= String (Text.pack $ show stok)
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
      , "sigids" .= String (Text.pack $ show sigids)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (SigSubV2.MsgReplySigs sigs)) =
    mconcat
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (Text.pack $ show stok)
      , "sigs" .= String (Text.pack $ show sigs)
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

instance (Show txid, Show tx)
      => Logging.LogFormatting (AnyMessage (TxSubmission2 txid tx)) where
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgInit) =
    mconcat
      [ "kind" .= String "MsgInit"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgRequestTxIds {}) =
    mconcat
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxIds _)) =
    mconcat
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgRequestTxs txids)) =
    mconcat
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (Text.pack $ show stok)
      , "txIds" .= String (Text.pack $ show txids)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxs txs)) =
    mconcat
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (Text.pack $ show stok)
      , "txs" .= String (Text.pack $ show txs)
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

instance Logging.LogFormatting (SigSubV2.TraceSigSubmissionOutbound SigId (Sig crypto)) where
  forMachine _ (SigSubV2.TraceSigSubmissionOutboundRecvMsgRequestSigs sigids) =
    mconcat [ "kind" .= String "TraceSigSubmissionOutboundRecvMsgRequestSigs"
            , "sigids" .= sigids
            ]
  forMachine _ (SigSubV2.TraceSigSubmissionOutboundSendMsgReplySigs sigs) =
    mconcat [ "kind" .= String "TraceSigSubmissionOutboundSendMsgReplySigs"
            , "sigids" .= (SigSubV2.sigId <$> sigs)
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

-- An orphan instance needed for `Handshake versionNumber Term`
instance ToJSON Term where
  toJSON term = String (Text.pack . show $ term)

instance ToJSON (PublicRootPeers (NoExtraPeers RemoteAddress) RemoteAddress) where
  toJSON prp =
    object [ "kind"              .= String "PublicRootPeers"
           , "ledgerPeers"       .= PublicRootPeers.getLedgerPeers prp
           , "bigLedgerPeers"    .= PublicRootPeers.getBigLedgerPeers prp
           ]

data NoExtraState      = NoExtraState
data NoExtraCounters   = NoExtraCounters   deriving Eq
data NoExtraDebugState = NoExtraDebugState
instance ToJSON NoExtraDebugState where
  toJSON _ = Null
  omitField _ = True
data NoExtraChurnArgs  = NoExtraChurnArgs
data NoExtraAPI        = NoExtraAPI
data NoExtraTracer     = NoExtraTracer
instance Show NoExtraState where
  show _ = ""
instance Show NoExtraDebugState where
  show _ = ""
instance Show NoExtraTracer where
  show _ = ""
instance ToJSON NoExtraTracer where
  toJSON _ = Null
  omitField _ = True
