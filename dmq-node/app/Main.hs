{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

module Main where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (void, when)
import Control.Monad.Class.MonadThrow
import "contra-tracer" Control.Tracer (traceWith)

import Data.Act
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Contravariant ((>$<))
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Data.Void (Void)
import Options.Applicative
import System.Exit (exitSuccess)
import System.Random (newStdGen, split)
import System.IOManager (withIOManager)

import Cardano.Git.Rev (gitRev)
import Cardano.KESAgent.Protocols.StandardCrypto (StandardCrypto)

import DMQ.Configuration
import DMQ.Configuration.CLIOptions (parseCLIOptions)
import DMQ.Configuration.Topology (readTopologyFileOrError)
import DMQ.Diffusion.Applications (diffusionApplications)
import DMQ.Diffusion.Arguments
import DMQ.Diffusion.NodeKernel
import DMQ.Handlers.TopLevel (toplevelExceptionHandler)
import DMQ.NodeToClient qualified as NtC
import DMQ.NodeToNode (NodeToNodeVersion, dmqCodecs, dmqLimitsAndTimeouts,
           ntnApps)
import DMQ.Protocol.SigSubmission.Type (Sig (..))
import DMQ.Tracer
  ( mkCardanoTracer
  , WithEventType (WithEventType), EventType (DMQ)
  )
import DMQ.Diffusion.PeerSelection (policy)
import DMQ.NodeToClient.LocalStateQueryClient
import DMQ.Protocol.SigSubmission.Validate
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress,
           encodeRemoteAddress)
import Ouroboros.Network.SizeInBytes
import Ouroboros.Network.Snocket
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool

import Paths_dmq_node qualified as Meta

main :: IO ()
main = toplevelExceptionHandler $ void . runDMQ =<< execParser opts
  where
    opts = info (parseCLIOptions <**> helper)
                ( fullDesc
                <> progDesc "Run the POC DMQ node"
                )

runDMQ :: PartialConfig -> IO Void
runDMQ commandLineConfig = do
    -- get the configuration file path
    let configFilePath = unI
                       $ dmqcConfigFile commandLineConfig
                   `act` dmqcConfigFile defaultConfiguration

    -- read & parse configuration file
    config' <- readConfigurationFileOrError configFilePath
    -- combine default configuration, configuration file and command line
    -- options
    let dmqConfig@Configuration {
          dmqcTopologyFile          = I topologyFile,
          dmqcCardanoNodeSocket     = I snocketPath,
          dmqcVersion               = I version
        } = config' <> commandLineConfig
            `act`
            defaultConfiguration

    (tracer, dmqDiffusionTracers) <- mkCardanoTracer configFilePath

    when version $ do
      let gitrev = $(gitRev)
          cleanGitRev = if | Text.take 6 (Text.drop 7 gitrev) == "-dirty"
                           -- short dirty revision
                           -> Just $ Text.take (6 + 7) gitrev
                           | Text.all (== '0') gitrev
                           -- no git revision available
                           -> Nothing
                           | otherwise
                           -> Just gitrev
      Text.putStr $ Text.unlines $
          "dmq-node version: " <> Text.pack (showVersion Meta.version)
        : [ "git revision: " <> rev
          | rev <- maybeToList cleanGitRev
          ]
      exitSuccess

    traceWith tracer (WithEventType (DMQ "Configuration") dmqConfig)
    nt <- readTopologyFileOrError topologyFile
    traceWith tracer (WithEventType (DMQ "NetworkTopology") nt)

    stdGen <- newStdGen
    let (psRng, policyRng) = split stdGen
    policyRngVar <- newTVarIO policyRng

    -- TODO: this might not work, since `ouroboros-network` creates its own IO Completion Port.
    withIOManager \iocp -> do
      let localSnocket'      = localSnocket iocp
          mkStakePoolMonitor = connectToCardanoNode
                                 (WithEventType (DMQ "LocalStateQuery") >$< tracer)
                                 localSnocket'
                                 snocketPath

      withNodeKernel @StandardCrypto
                     tracer
                     dmqConfig
                     psRng
                     mkStakePoolMonitor $ \nodeKernel -> do
        dmqDiffusionConfiguration <-
          mkDiffusionConfiguration dmqConfig nt nodeKernel.stakePools.ledgerBigPeersVar

        let sigSize :: Sig StandardCrypto -> SizeInBytes
            sigSize = fromIntegral . BSL.length . sigRawBytes
            mempoolReader = Mempool.getReader sigId sigSize (mempool nodeKernel)
            ntnValidationTracer = (WithEventType (DMQ "NtC Validation") >$< tracer)
            dmqNtNApps =
              let ntnMempoolWriter =
                    Mempool.getWriter SigDuplicate
                                      sigId
                                      (\now sigs ->
                                        withPoolValidationCtx (stakePools nodeKernel) (validateSig now sigs)
                                      )
                                      (traverse_ $ \(sigid, reason) -> do
                                        traceWith ntnValidationTracer $ InvalidSignature sigid reason
                                        case reason of
                                          SigDuplicate      -> return ()
                                          SigExpired        -> return ()
                                          NotInitialized    -> return ()
                                          PoolNotEligible   -> return ()
                                          UnrecognizedPool  -> return ()
                                          err               -> throwIO (SigValidationException sigid err)
                                      )
                                      (mempool nodeKernel)
               in ntnApps tracer
                          dmqConfig
                          mempoolReader
                          ntnMempoolWriter
                          sigSize
                          nodeKernel
                          (dmqCodecs
                                   -- TODO: `maxBound :: Cardano.Network.NodeToNode.NodeToNodeVersion`
                                   -- is unsafe here!
                                   (encodeRemoteAddress (maxBound @NodeToNodeVersion))
                                   (decodeRemoteAddress (maxBound @NodeToNodeVersion)))
                          dmqLimitsAndTimeouts
                          defaultSigDecisionPolicy
            ntcValidationTracer = (WithEventType (DMQ "NtC Validation") >$< tracer)
            dmqNtCApps =
              let ntcMempoolWriter =
                    Mempool.getWriter SigDuplicate
                                      sigId
                                      (\now sigs ->
                                        withPoolValidationCtx (stakePools nodeKernel) (validateSig now sigs)
                                      )
                                      (traverse_ $ \(sigid, reason) ->
                                         traceWith ntcValidationTracer $ InvalidSignature sigid reason
                                      )
                                      (mempool nodeKernel)
               in NtC.ntcApps tracer dmqConfig
                              mempoolReader ntcMempoolWriter
                              NtC.dmqCodecs
            dmqDiffusionArguments =
              diffusionArguments (WithEventType (DMQ "Handshake") >$< tracer)
                                 (WithEventType (DMQ "LocalHandshake") >$< tracer)
                                 $ maybe [] out <$> tryReadTMVar nodeKernel.stakePools.ledgerPeersVar
              where
                out :: LedgerPeerSnapshot AllLedgerPeers
                    -> [(PoolStake, NonEmpty LedgerRelayAccessPoint)]
                out (LedgerAllPeerSnapshotV23 _pt _magic relays) = relays

            dmqDiffusionApplications =
              diffusionApplications nodeKernel
                                    dmqConfig
                                    dmqDiffusionConfiguration
                                    dmqLimitsAndTimeouts
                                    dmqNtNApps
                                    dmqNtCApps
                                    (policy policyRngVar)

        Diffusion.run dmqDiffusionArguments
                      dmqDiffusionTracers
                      dmqDiffusionConfiguration
                      dmqDiffusionApplications
