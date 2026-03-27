{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DMQ.Configuration.Topology
  ( readTopologyFileOrError
  , NoExtraFlags (..)
  , NoExtraConfig (..)
  ) where

import Control.Exception (Exception (..), IOException, try)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Ouroboros.Network.Diffusion.Topology (NetworkTopology (..))
import Ouroboros.Network.Diffusion.Types (NoExtraFlags (..))
import Ouroboros.Network.OrphanInstances (localRootPeersGroupsFromJSON,
           networkTopologyFromJSON, networkTopologyToJSON)
import System.Exit (die)

-- TODO: move `NoExtraConfig` and the `ToJSON NoExtraFlags` to
-- `ouroboros-network`.

data NoExtraConfig = NoExtraConfig
  deriving Show

instance ToJSON NoExtraFlags where
  toJSON _ = Null
  omitField _ = True

instance FromJSON (NetworkTopology NoExtraConfig NoExtraFlags) where
  parseJSON = networkTopologyFromJSON
                (localRootPeersGroupsFromJSON (\_ -> pure NoExtraFlags))
                (\_ -> pure NoExtraConfig)

instance ToJSON (NetworkTopology NoExtraConfig NoExtraFlags) where
  toJSON = networkTopologyToJSON (const Nothing) (const Nothing)

-- | Read the `NetworkTopology` configuration from the specified file.
--
readTopologyFile
  :: FilePath
  -> IO (Either Text (NetworkTopology NoExtraConfig NoExtraFlags))
readTopologyFile nc = do
  eBs <- try $ BS.readFile nc

  case eBs of
    Left e -> return . Left $ handler e
    Right bs ->
      let bs' = LBS.fromStrict bs in
        case eitherDecode bs' of
          Left err -> return $ Left (handlerJSON err)
          Right t  -> return $ Right t
  where
    handler :: IOException -> Text
    handler e = Text.pack $ "DMQ.Topology.readTopologyFile: "
                          ++ displayException e
    handlerJSON :: String -> Text
    handlerJSON err = Text.unlines
      [ "topology parsing error:"
      , Text.pack err
      ]

readTopologyFileOrError
  :: FilePath
  -> IO (NetworkTopology NoExtraConfig NoExtraFlags)
readTopologyFileOrError nc =
      readTopologyFile nc
  >>= either (die . Text.unpack)
             pure
