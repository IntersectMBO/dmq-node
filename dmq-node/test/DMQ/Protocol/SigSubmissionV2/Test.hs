{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module DMQ.Protocol.SigSubmissionV2.Test
  ( tests
  , SigId (..)
  , Sig (..)
  ) where

import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.ST (runST)
import Codec.CBOR.FlatTerm qualified as CBOR
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Generics

import Codec.Serialise (DeserialiseFailure, Serialise)
import Codec.Serialise qualified as Serialise (decode, encode)

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.Properties (prop_codecM, prop_codec_splitsM)

import Ouroboros.Network.Util.ShowProxy

import DMQ.Protocol.SigSubmissionV2.Codec
import DMQ.Protocol.SigSubmissionV2.Type hiding (Sig, SigId)

import Test.Data.CDDL (Any (..))
import Test.Ouroboros.Network.Protocol.Utils (prop_codec_cborM,
           prop_codec_valid_cbor_encoding, splits2, splits3)
import Test.Ouroboros.Network.Utils (renderRanges)

import Test.QuickCheck as QC
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


--
-- Test cases
--


tests :: TestTree
tests =
  testGroup "DMQ.Protocol"
    [ testGroup "SigSubmissionV2"
        [ testProperty "codec"               prop_codec
        , testProperty "encoding"            prop_encoding
        , testProperty "codec id"            prop_codec_id
        , testProperty "codec 2-splits"    $ withMaxSize 50
                                             prop_codec_splits2
        , testProperty "codec 3-splits"    $ withMaxSize 10
                                             prop_codec_splits3
        , testProperty "codec cbor"          prop_codec_cbor
        , testProperty "codec valid cbor"    prop_codec_valid_cbor
        ]
    ]

--
-- Common types & clients and servers used in the tests in this module.
--

newtype Sig = Sig SigId
  deriving (Eq, Show, Arbitrary, Serialise, Generic)

instance ShowProxy Sig where
    showProxy _ = "Sig"

-- | We use any `CBOR.Term`.  This allows us to use `any` in cddl specs.
--
newtype SigId = SigId Any
  deriving (Eq, Ord, Show, Arbitrary, Serialise, Generic)

instance ShowProxy SigId where
    showProxy _ = "SigId"

deriving newtype instance Arbitrary SizeInBytes

deriving newtype instance Arbitrary NumIdsAck
deriving newtype instance Arbitrary NumIdsReq

instance Arbitrary (AnyMessage (SigSubmissionV2 SigId Sig)) where
  arbitrary = oneof
    [ AnyMessage
        <$> ( MsgRequestSigIds SingBlocking
            <$> arbitrary
            <*> arbitrary
            )

    , AnyMessage
        <$> ( MsgRequestSigIds SingNonBlocking
            <$> arbitrary
            <*> arbitrary
            )

    , AnyMessage
        <$> MsgReplySigIds
        <$> ( BlockingReply
            . NonEmpty.fromList
            . QC.getNonEmpty
            )
        <$> arbitrary

    , AnyMessage
        <$> MsgReplySigIds
        <$> NonBlockingReply
        <$> arbitrary

    , AnyMessage
        <$> pure MsgReplyNoSigIds

    , AnyMessage
        <$> MsgRequestSigs
        <$> arbitrary

    , AnyMessage
        <$> MsgReplySigs
        <$> arbitrary

    , AnyMessage
        <$> pure MsgDone
    ]

instance (Eq sigId
         , Eq sig
         )
      => Eq (AnyMessage (SigSubmissionV2 sigId sig)) where

  (==) (AnyMessage (MsgRequestSigIds SingBlocking ackNo  reqNo))
       (AnyMessage (MsgRequestSigIds SingBlocking ackNo' reqNo')) =
    (ackNo, reqNo) == (ackNo', reqNo')

  (==) (AnyMessage (MsgRequestSigIds SingNonBlocking ackNo  reqNo))
       (AnyMessage (MsgRequestSigIds SingNonBlocking ackNo' reqNo')) =
    (ackNo, reqNo) == (ackNo', reqNo')

  (==) (AnyMessage (MsgReplySigIds (BlockingReply sigIds)))
       (AnyMessage (MsgReplySigIds (BlockingReply sigIds'))) =
    sigIds == sigIds'

  (==) (AnyMessage (MsgReplySigIds (NonBlockingReply sigIds)))
       (AnyMessage (MsgReplySigIds (NonBlockingReply sigIds'))) =
    sigIds == sigIds'

  (==) (AnyMessage MsgReplyNoSigIds)
       (AnyMessage MsgReplyNoSigIds) = True

  (==) (AnyMessage (MsgRequestSigs sigIds))
       (AnyMessage (MsgRequestSigs sigIds')) = sigIds == sigIds'

  (==) (AnyMessage (MsgReplySigs txs))
       (AnyMessage (MsgReplySigs txs')) = txs == txs'

  (==) (AnyMessage MsgDone)
       (AnyMessage MsgDone) = True

  (==) _ _ = False


codec :: MonadST m
      => Codec
           (SigSubmissionV2 SigId Sig)
           DeserialiseFailure
           m ByteString
codec = codecSigSubmissionV2
          Serialise.encode Serialise.decode
          Serialise.encode Serialise.decode


-- | Check the codec round trip property.
--
prop_codec
  :: AnyMessage (SigSubmissionV2 SigId Sig)
  -> Property
prop_codec msg =
  runST (prop_codecM codec msg)


prop_encoding :: AnyMessage (SigSubmissionV2 SigId Sig)
              -> Property
prop_encoding msg@(AnyMessage msg') = 
  let enc = encodeSigSubmissionV2 Serialise.encode Serialise.encode msg'
      terms = CBOR.toFlatTerm enc
  in counterexample (show msg)
   . counterexample ("terms: " ++ show terms)
   $ CBOR.validFlatTerm terms


-- | Check the codec round trip property for the id codec.
--
prop_codec_id
  :: AnyMessage (SigSubmissionV2 SigId Sig)
  -> Property
prop_codec_id msg =
  runST (prop_codecM codecSigSubmissionV2Id msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2
  :: AnyMessage (SigSubmissionV2 SigId Sig)
  -> Property
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 codec msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3
  :: AnyMessage (SigSubmissionV2 SigId Sig)
  -> Property
prop_codec_splits3 msg =
  labelMsg msg $
  runST (prop_codec_splitsM splits3 codec msg)

prop_codec_cbor
  :: AnyMessage (SigSubmissionV2 SigId Sig)
  -> Property
prop_codec_cbor msg =
  runST (prop_codec_cborM codec msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: AnyMessage (SigSubmissionV2 SigId Sig)
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codec


labelMsg :: AnyMessage (SigSubmissionV2 sigId sig) -> Property -> Property
labelMsg (AnyMessage msg) =
  label (case msg of
           MsgRequestSigIds {} -> "MsgRequestSigIds"
           MsgReplySigIds as   -> "MsgReplySigIds " ++ renderRanges 3 (length as)
           MsgReplyNoSigIds    -> "MsgReplyNoSigIds"
           MsgRequestSigs as   -> "MsgRequestSigs " ++ renderRanges 3 (length as)
           MsgReplySigs as     -> "MsgReplySigs " ++ renderRanges 3 (length as)
           MsgDone             -> "MsgDone"
        )
