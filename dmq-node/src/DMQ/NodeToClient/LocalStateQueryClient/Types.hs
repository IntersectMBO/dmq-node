{-# LANGUAGE OverloadedStrings #-}

module DMQ.NodeToClient.LocalStateQueryClient.Types (TraceLocalStateQueryClient (..)) where

import Control.Monad.Class.MonadTime.SI
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson

import Cardano.Logging qualified as Logging
import Cardano.Slotting.Slot (EpochNo)
import Cardano.Slotting.Time (SystemStart)
import Ouroboros.Consensus.HardFork.History.Qry (PastHorizonException)

-- | Trace type
--
data TraceLocalStateQueryClient =
    Acquiring (Maybe SystemStart)
  | CurrentEpoch EpochNo
  | NextEpoch UTCTime NominalDiffTime
  | PastHorizon PastHorizonException
  | LedgerPeersNotAvailable

instance Logging.LogFormatting TraceLocalStateQueryClient where
  forMachine _dtal = \case
    Acquiring mSystemStart ->
      mconcat [ "kind" .= Aeson.String "Acquiring"
              , "systemStart" .= show mSystemStart
              ]
    CurrentEpoch epoch ->
      mconcat [ "kind" .= Aeson.String "CurrentEpoch"
              , "remoteEpoch" .= show epoch
              ]
    NextEpoch ne timer ->
      mconcat [ "kind" .= Aeson.String "NextEpoch"
              , "remoteTime" .= show ne
              , "countdown" .= show timer ]
    PastHorizon e ->
      mconcat [ "kind" .= Aeson.String "PastHorizon"
              , "error" .= show e
              ]
    LedgerPeersNotAvailable ->
      mconcat [ "kind" .= Aeson.String "LedgerPeersNotAvailable" ]

instance Logging.MetaTrace TraceLocalStateQueryClient where
    namespaceFor Acquiring{}               = Logging.Namespace [] ["Acquiring"]
    namespaceFor CurrentEpoch{}            = Logging.Namespace [] ["CurrentEpoch"]
    namespaceFor NextEpoch{}               = Logging.Namespace [] ["NextEpoch"]
    namespaceFor PastHorizon{}             = Logging.Namespace [] ["PastHorizon"]
    namespaceFor LedgerPeersNotAvailable{} = Logging.Namespace [] ["LedgerPeersNotAvailable"]

    severityFor _ (Just Acquiring{})               = Just Logging.Debug
    severityFor _ (Just CurrentEpoch{})            = Just Logging.Info
    severityFor _ (Just NextEpoch{})               = Just Logging.Info
    severityFor _ (Just PastHorizon{})             = Just Logging.Error
    severityFor _ (Just LedgerPeersNotAvailable{}) = Just Logging.Warning
    severityFor _ Nothing                          = Nothing

    documentFor _ = Nothing
    allNamespaces =
      [ Logging.Namespace [] ["Acquiring"]
      , Logging.Namespace [] ["CurrentEpoch"]
      , Logging.Namespace [] ["NextEpoch"]
      , Logging.Namespace [] ["PastHorizon"]
      , Logging.Namespace [] ["LedgerPeersNotAvailable"]
      ]

