{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

module Main where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (unless, void, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import "contra-tracer" Control.Tracer (nullTracer, traceWith)

import Data.Act
import Data.ByteString.Lazy qualified as BSL
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Functor.Contravariant ((>$<))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Data.Void (Void)
import Options.Applicative
import System.Directory qualified as Dir
import System.Exit (die, exitSuccess)
import System.IOManager (withIOManager)
import System.Metrics qualified as EKG
import System.Random qualified as Random

import Cardano.Git.Rev (gitRev)
import Cardano.KESAgent.Protocols.StandardCrypto (StandardCrypto)
import Cardano.Logging.Prometheus.TCPServer qualified as Prometheus

import DMQ.Configuration
import DMQ.Configuration.CLIOptions (parseCLIOptions)
import DMQ.Configuration.Topology (readTopologyFileOrError)
import DMQ.Diffusion.Applications (diffusionApplications)
import DMQ.Diffusion.Arguments
import DMQ.Diffusion.NodeKernel
import DMQ.Diffusion.PeerSelection (policy)
import DMQ.Handlers.TopLevel (toplevelExceptionHandler)
import DMQ.NodeToClient qualified as NtC
import DMQ.NodeToClient.LocalStateQueryClient
import DMQ.NodeToNode (NodeToNodeVersion, dmqCodecs, dmqLimitsAndTimeouts,
           ntnApps)
import DMQ.Policy qualified as Policy
import DMQ.Protocol.SigSubmission.Type (Sig (..))
import DMQ.Protocol.SigSubmission.Validate
import DMQ.Tracer (DMQStartupTrace (..), DMQTracers (..), mkDMQTracers)
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
                <> progDesc "Run the DMQ-Node"
                )

runDMQ :: PartialConfig -> IO Void
runDMQ commandLineConfig = do
    -- get the configuration file path
    let configFilePath = unI
                       $ dmqcConfigFile commandLineConfig
                   `act` dmqcConfigFile defaultConfiguration

    ekgStore <- EKG.newStore
    EKG.registerGcMetrics ekgStore

    -- read & parse configuration file
    config' <- readConfigurationFile configFilePath
    -- combine default configuration, configuration file and command line
    -- options
    let dmqConfig :: Configuration
        dmqConfig@Configuration {
          dmqcTopologyFile          = I topologyFile,
          dmqcCardanoNodeSocket     = I socketPath,
          dmqcVersion               = I version,
          dmqcLedgerPeers           = I ledgerPeers
        } =     fromRight mempty config'
             <> commandLineConfig
            `act`
            defaultConfiguration

    when version $ do
      let gitrev :: Text.Text
          gitrev = $(gitRev)

          cleanGitRev :: Maybe Text.Text
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

    (   dmqTracers@DMQTracers {
          dmqStartupTracer,
          localStateQueryClientTracer,
          sigValidationTracer,
          localSigValidationTracer,
          cardanoNodeHandshakeTracer
        }
      , dmqDiffusionTracers
      , prometheusConfig
      )
      <- mkDMQTracers ekgStore configFilePath

    case prometheusConfig of
      Nothing -> return ()
      Just ps ->
        -- morally it belongs to `NodeKernel`, but it runs in `IO`, not `m`.
        Prometheus.runPrometheusSimple
          (DMQPrometheus >$< dmqStartupTracer)
          ekgStore ps
          >>= link

    traceWith dmqStartupTracer (DMQConfiguration dmqConfig)
    Dir.doesFileExist socketPath >>= \a ->
      unless a (die $ "CardanoNodeSocket " ++ show socketPath ++": file does not exist")
    nt <- readTopologyFileOrError topologyFile
    traceWith dmqStartupTracer (DMQTopology nt)

    stdGen <- Random.newStdGen
    let (psRng, policyRng) = Random.splitGen stdGen
    policyRngVar <- newTVarIO policyRng

    -- TODO: this might not work, since `ouroboros-network` creates its own IO Completion Port.
    withIOManager \iocp -> do
      let localSnocket'      = localSnocket iocp
          mkStakePoolMonitor = connectToCardanoNode
                                 localStateQueryClientTracer
                                 ledgerPeers
                                 localSnocket'
                                 socketPath

      withNodeKernel @StandardCrypto
                     dmqTracers
                     dmqConfig
                     psRng
                     mkStakePoolMonitor $ \nodeKernel -> do
        dmqDiffusionConfiguration <-
          mkDiffusionConfiguration dmqConfig nt nodeKernel.stakePools.ledgerBigPeersVar

        let sigSize :: Sig StandardCrypto -> SizeInBytes
            sigSize = fromIntegral . BSL.length . sigRawBytes
            mempoolReader = Mempool.getReader sigId sigSize (mempool nodeKernel)
            dmqNtNApps =
              let ntnMempoolWriter =
                    Mempool.getWriter SigDuplicate
                                      sigId
                                      (\now sigs ->
                                        withPoolValidationCtx (stakePools nodeKernel) (validateSig now sigs)
                                      )
                                      (traverse_ $ \(sigid, reason) -> do
                                        traceWith sigValidationTracer $ InvalidSignature sigid reason
                                        case reason of
                                          SigDuplicate      -> return ()
                                          SigExpired        -> return ()
                                          NotInitialized    -> return ()
                                          PoolNotEligible   -> return ()
                                          UnrecognizedPool  -> return ()
                                          err               -> throwIO (SigValidationException sigid err)
                                      )
                                      (mempool nodeKernel)
               in ntnApps dmqTracers
                          dmqConfig
                          mempoolReader
                          ntnMempoolWriter
                          sigSize
                          nodeKernel
                          (dmqCodecs
                             (encodeRemoteAddress (maxBound @NodeToNodeVersion))
                             (decodeRemoteAddress (maxBound @NodeToNodeVersion)))
                          dmqLimitsAndTimeouts
                          Policy.sigDecisionPolicy
            dmqNtCApps =
              let ntcMempoolWriter =
                    Mempool.getWriter SigDuplicate
                                      sigId
                                      (\now sigs ->
                                        withPoolValidationCtx (stakePools nodeKernel) (validateSig now sigs)
                                      )
                                      (traverse_ $ \(sigid, reason) ->
                                         traceWith localSigValidationTracer $ InvalidSignature sigid reason
                                      )
                                      (mempool nodeKernel)
               in NtC.ntcApps dmqTracers dmqConfig
                              mempoolReader ntcMempoolWriter
                              NtC.dmqCodecs
            dmqDiffusionArguments =
              diffusionArguments nullTracer
                                 cardanoNodeHandshakeTracer
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
