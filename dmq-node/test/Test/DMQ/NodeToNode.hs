{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.NodeToNode (tests) where

import Data.Foldable (fold)

import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.DiffusionMode
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux qualified as Mux
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))

import DMQ.NodeToNode

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "DMQ.Network.NodeToNode"
    [ testProperty "nodeToNodeVersionCodec"  prop_nodeToNodeVersionCodec
    , testProperty "nodeToNodeCodecCBORTerm" prop_nodeToNodeCodec
    , testProperty "mini-protocol ingress limits" prop_nodeToNodeIngressLimits
    ]

instance Arbitrary NodeToNodeVersion where
    arbitrary = arbitraryBoundedEnum

    shrink v
      | v == minBound = []
      | otherwise     = [pred v]

instance Arbitrary NodeToNodeVersionData where
    arbitrary =
      NodeToNodeVersionData
        <$> (NetworkMagic <$> arbitrary)
        <*> oneof [ pure InitiatorOnlyDiffusionMode
                  , pure InitiatorAndResponderDiffusionMode
                  ]
        <*> elements [ PeerSharingDisabled
                     , PeerSharingEnabled
                     ]
        <*> arbitrary


prop_nodeToNodeVersionCodec :: NodeToNodeVersion
                            -> Bool
prop_nodeToNodeVersionCodec version =
    case decodeTerm (encodeTerm version) of
        Right version' -> version == version'
        Left {}        -> False
  where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToNodeVersionCodec


prop_nodeToNodeCodec :: NodeToNodeVersion -> NodeToNodeVersionData -> Bool
prop_nodeToNodeCodec ntnVersion ntnData =
      case decodeTerm (encodeTerm ntnData) of
        Right ntnData' -> networkMagic  ntnData' == networkMagic  ntnData
                       && diffusionMode ntnData' == diffusionMode ntnData
        Left {}        -> False
    where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToNodeCodecCBORTerm ntnVersion


prop_nodeToNodeIngressLimits :: NodeToNodeVersion -> NodeToNodeVersionData -> Property
prop_nodeToNodeIngressLimits version versionData =
    [ (num, Mux.maximumIngressQueue initiatorLimits, Mux.maximumIngressQueue responderLimits)
    | protocol <- fold bundle
    , let Mux.MiniProtocolNum num = Mux.miniProtocolNum protocol
          Mux.DirectionalLimits { Mux.initiatorLimits, Mux.responderLimits } = Mux.miniProtocolLimits protocol
    ] === [sigLimits, (12, 1_408, 1_408)]
       ++ [(13, 5_760, 1_584) | peerSharing versionData == PeerSharingEnabled]
  where
    -- Signature replies need room for 33 signatures of 2,800 bytes plus 10%.
    sigLimits = case version of
      NodeToNodeV_1 -> (11, 72_088, 101_640)
      NodeToNodeV_2 -> (11, 101_640, 72_088)

    bundle = nodeToNodeProtocols dmqLimitsAndTimeouts (Protocols run run run) version versionData
    run = Mux.InitiatorAndResponderProtocol peer peer
    peer = Mux.MiniProtocolCb $ \_ _ -> pure ((), Nothing) :: IO ((), Maybe ())

