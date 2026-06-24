{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.SigSubmission.Types
  ( TestVersion (..)
  , WellSizedSig (..)
  , SigSubmissionState (..)
  , SigStateTrace (..)
  , sigSubmissionCodec2
    -- * Re-exports
  , SizeInBytes (..)
  ) where

import Prelude hiding (seq)

import Control.Monad.Class.MonadST

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR

import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import Data.List (nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Network.TypedProtocol.Codec

import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V2 (SharedTxState,
           TxDecisionPolicy)

import DMQ.Protocol.SigSubmissionV2.Codec (codecSigSubmissionV2)
import DMQ.Protocol.SigSubmissionV2.Type (SigSubmissionV2)

import Test.Ouroboros.Network.TxSubmission.TxLogic (ArbTxDecisionPolicy (..))
import Test.Ouroboros.Network.TxSubmission.Types (Tx (..), TxId)
import Test.Ouroboros.Network.Utils (SmallDelay)
import Test.QuickCheck


data TestVersion = TestVersion
  deriving (Eq, Ord, Bounded, Enum, Show)


newtype WellSizedSig = WellSizedSig { getSig :: Tx TxId }
  deriving Show

fixupWellSizedTx :: Tx TxId -> Tx TxId
fixupWellSizedTx tx@Tx { getTxSize } = tx { getTxAdvSize = getTxSize }

instance Arbitrary WellSizedSig where
  arbitrary =  WellSizedSig
            .  fixupWellSizedTx
           <$> arbitrary
  shrink = map (WellSizedSig . fixupWellSizedTx)
         . shrink
         . getSig


sigSubmissionCodec2 :: MonadST m
                   => Codec (SigSubmissionV2 TxId (Tx Int))
                            CBOR.DeserialiseFailure m ByteString
sigSubmissionCodec2 =
    codecSigSubmissionV2 CBOR.encodeInt CBOR.decodeInt
                         encodeSig decodeSig
  where
    encodeSig :: Tx TxId -> CBOR.Encoding
    encodeSig Tx {getTxId, getTxSize, getTxAdvSize, getTxValid} =
         CBOR.encodeListLen 4
      <> CBOR.encodeInt getTxId
      <> CBOR.encodeWord32 (getSizeInBytes getTxSize)
      <> CBOR.encodeWord32 (getSizeInBytes getTxAdvSize)
      <> CBOR.encodeBool getTxValid

    decodeSig :: CBOR.Decoder s (Tx TxId)
    decodeSig = do
      _ <- CBOR.decodeListLen
      Tx <$> CBOR.decodeInt
         <*> (SizeInBytes <$> CBOR.decodeWord32)
         <*> (SizeInBytes <$> CBOR.decodeWord32)
         <*> CBOR.decodeBool
         <*> pure Nothing


data SigSubmissionState =
  SigSubmissionState {
      peerMap :: Map Int ( [Tx Int]
                         -- ^ mempool state
                         , Maybe (Positive SmallDelay)
                         , Maybe (Positive SmallDelay)
                         -- ^ The delay must be smaller (<) than 5s, so that overall
                         -- delay is less than 10s, otherwise 'smallDelay' in
                         -- 'timeLimitsTxSubmission2' will kick in.
                         )
    , decisionPolicy :: TxDecisionPolicy
  } deriving (Show)

instance Arbitrary SigSubmissionState where
  arbitrary = do
    ArbTxDecisionPolicy decisionPolicy <- arbitrary
    peersN <- choose (1, 10)
    txsN <- frequency [ (1, choose (1, 10))
                      , (2, choose (10, 20))
                      , (4, choose (20, 100))
                      ]

    -- NOTE: using sortOn would forces tx-decision logic to download txs in the
    -- order of unacknowledgedTxIds.  This could be useful to get better
    -- properties when wrongly sized txs are present.
    txs <- divvy txsN . nubBy (on (==) getTxId) {- . List.sortOn getTxId -} <$> vectorOf (peersN * txsN) arbitrary
    peers <- vectorOf peersN arbitrary
    peersState <- zipWith (\a (b, c) -> (a, b, c)) txs
              <$> vectorOf peersN arbitrary
    return SigSubmissionState  { peerMap = Map.fromList (zip peers peersState),
                                 decisionPolicy
                               }
    where
      -- | Split a list into sub list of at most `n` elements.
      --
      divvy :: Int -> [a] -> [[a]]
      divvy _ [] = []
      divvy n as = take n as : divvy n (drop n as)

  shrink SigSubmissionState { peerMap, decisionPolicy } =
    SigSubmissionState
      <$> shrinkMapBy Map.fromList Map.toList (shrinkList shrink) peerMap
      <*> [ policy
          | ArbTxDecisionPolicy policy <- shrink (ArbTxDecisionPolicy decisionPolicy)
          ]

newtype SigStateTrace peeraddr sigid =
    SigStateTrace (SharedTxState peeraddr sigid (Tx sigid))
