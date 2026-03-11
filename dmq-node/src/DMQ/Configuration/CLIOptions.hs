module DMQ.Configuration.CLIOptions (parseCLIOptions) where

import Data.Monoid (Last (..))
import Options.Applicative
import Options.Applicative.Help qualified as Help

import DMQ.Configuration
import Ouroboros.Network.Magic (NetworkMagic (..))

parseCLIOptions :: Parser PartialConfig
parseCLIOptions =
  mkConfiguration
    <$> optional (
          option auto
          (  long "host-addr"
          <> metavar "IPv4"
          <> help "IPv4 that the node will bind to"
          )
        )
    <*> optional (
          option auto
           (  long "host-ipv6-addr"
           <> metavar "IPv6"
           <> help "IPv6 that the node will bind to"
           )
        )
    <*> optional (
          option auto
          (  long "port"
          <> short 'p'
          <> metavar "Port Number"
          <> helpWithDefault
               (unI $ dmqcPortNumber defaultConfiguration)
               "Port Number that the node will bind to"
          )
        )
    <*> optional (
          strOption
          (  long "local-socket"
          <> metavar "FILENAME"
          <> helpWithDefault
              (getFilePath . unI $ dmqcLocalAddress defaultConfiguration)
              "Unix socket for node-to-client communication"
          )
        )
    <*> optional (
          strOption
          (  long "configuration-file"
          <> short 'c'
          <> metavar "FILENAME"
          <> helpWithDefault
               (unI $ dmqcConfigFile defaultConfiguration)
               "Configuration file for DMQ Node"
          )
        )
    <*> optional (
          strOption
          (  long "topology-file"
          <> short 't'
          <> metavar "FILENAME"
          <> helpWithDefault
               (unI $ dmqcTopologyFile defaultConfiguration)
               "Topology file for DMQ Node"
          )
        )
    <*> optional (
          strOption
          (  long "cardano-node-socket"
          <> metavar "Cardano node socket path"
          <> helpWithDefault
               (unI $ dmqcCardanoNodeSocket defaultConfiguration)
               "Used for local connections to Cardano node"
          )
        )
    <*> optional (
          option auto
          (  long "cardano-network-magic"
          <> metavar "Cardano node network magic"
          <> helpWithDefault
              (unNetworkMagic . unI $ dmqcCardanoNetworkMagic defaultConfiguration)
              "The network magic of cardano-node client for local connections"
          )
        )
    <*> optional (
          option auto
          (  long "dmq-network-magic"
          <> metavar "dmq node network magic"
          <> helpWithDefault
              (unNetworkMagic . unI $ dmqcNetworkMagic defaultConfiguration)
              "The network magic of the dmq network"
          )
        )
    <*> optional (
          switch
          (   long "version"
          <>  short 'v'
          <> help "Show dmq-node version"
          )
        )
  where
    -- NOTE: we cannot simply use `value <> showDefault`, because configuration
    -- will always overwrite values provided by configuration file.
    helpWithDefault :: Show a
                    => a -> String -> Mod f a
    helpWithDefault a helpText =
      helpDoc . Just . Help.extractChunk $ Help.vcatChunks
        [ Help.paragraph helpText
        , Help.paragraph ("(default: " ++ show a ++ ")")
        ]

    mkConfiguration ipv4 ipv6 portNumber localAddress
                    configFile topologyFile cardanoNodeSocket cardanoNetworkMagic dmqNetworkMagic
                    version =
      mempty { dmqcIPv4                = Last (Just <$> ipv4),
               dmqcIPv6                = Last (Just <$> ipv6),
               dmqcLocalAddress        = Last (LocalAddress <$> localAddress),
               dmqcPortNumber          = Last portNumber,
               dmqcConfigFile          = Last configFile,
               dmqcTopologyFile        = Last topologyFile,
               dmqcCardanoNodeSocket   = Last cardanoNodeSocket,
               dmqcCardanoNetworkMagic = Last (NetworkMagic <$> cardanoNetworkMagic),
               dmqcNetworkMagic        = Last (NetworkMagic <$> dmqNetworkMagic),
               dmqcVersion             = Last version
             }
