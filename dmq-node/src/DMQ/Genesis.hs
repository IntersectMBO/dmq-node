{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}


-- Excerpt from `Cardano.Node.Protocol.Shelley` in `cardano-node` package.
module DMQ.Genesis
  ( GenesisHash (..)
  , GenesisFile (..)
  , ShelleyGenesis (..)
  , readGenesis
  ) where

import Cardano.Crypto.Hash.Blake2b qualified as Crypto
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..))

import Control.Exception (Exception (..), IOException)
import Control.Monad (forM_, when)
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT,
           hoistEither)

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.String (IsString)
import System.Directory (getFileSize)
import System.IO.MMap qualified as MMap


newtype GenesisHash = GenesisHash (Crypto.Hash Crypto.Blake2b_256 ByteString)
  deriving newtype (Eq, Show, ToJSON, FromJSON)

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

data GenesisReadError =
       GenesisReadFileError !FilePath !IOException
     | GenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
     | GenesisDecodeError !FilePath !String
  deriving Show

instance Exception GenesisReadError where
  displayException (GenesisReadFileError fp err) =
        "There was an error reading the genesis file: "
     <> show fp <> " Error: " <> show err

  displayException (GenesisHashMismatch actual expected) =
        "Wrong genesis file: the actual hash is " <> show actual
     <> ", but the expected genesis hash given in the node "
     <> "configuration file is " <> show expected

  displayException (GenesisDecodeError fp err) =
        "There was an error parsing the genesis file: "
     <> show fp <> " Error: " <> show err


readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO (ShelleyGenesis, GenesisHash)
readGenesis = readGenesisAny

-- | Read a genesis file using a memory-safe strategy.
--
-- Genesis files are used in testing and benchmarking to create testnets with
-- large datasets. To be able to read these files in memory constrained
-- environments we use a conditional loading strategy: if file is below ~10
-- megabytes it is entirely read into memory, otherwise the 'ByteString' is
-- created using `mmap` that uses the virtual memory subsystem to do on-demand
-- loading.
-- With current usage and how benchmakring is done only Shelley and Conway are
-- affected (Shelley's `initialFunds` and Conway's "delegs" and "initialDReps").
readGenesisAny :: FromJSON genesis
               => GenesisFile
               -> Maybe GenesisHash
               -> ExceptT GenesisReadError IO (genesis, GenesisHash)
readGenesisAny (GenesisFile file) mExpectedGenesisHash = do
    content <- handleIOExceptT (GenesisReadFileError file) $ do
        -- Size of the file in 8-bit bytes.
        size <- getFileSize file
        {-- Mainnet files for reference:
              -byron-genesis.json:   1056360 (1.1M)
              -shelley-genesis.json:    2486 (2.5K)
              -alonzo-genesis.json:     9459 (9.3K)
              -conway-genesis.json:     4168 (4.1K)
        --}
        if size >= 10485760 -- 10 megabytes.
        then MMap.mmapFileByteString file Nothing -- MMap version.
        else BS.readFile file                     -- Non-lazy version.
    genesisHash <- checkExpectedGenesisHash content mExpectedGenesisHash
    genesis <- firstExceptT (GenesisDecodeError file) $ hoistEither $
                 Aeson.eitherDecodeStrict' content
    return (genesis, genesisHash)

checkExpectedGenesisHash
  :: BS.ByteString -- ^ genesis bytes
  -> Maybe GenesisHash -- ^ expected hash, check for hash match, if provided
  -> ExceptT GenesisReadError IO GenesisHash
checkExpectedGenesisHash genesisBytes mExpected = do
  let actual = GenesisHash $ Crypto.hashWith id genesisBytes
  forM_ mExpected $ \expected ->
    when (actual /= expected) $
      throwError (GenesisHashMismatch actual expected)
  pure actual
