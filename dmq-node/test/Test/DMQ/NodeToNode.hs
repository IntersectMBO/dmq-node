{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.NodeToNode (tests) where

import Codec.CBOR.Term qualified as CBOR
import Control.Monad.Class.MonadAsync (concurrently)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import Data.Aeson qualified as Aeson
import Data.Foldable (fold)

import Ouroboros.Network.Channel (createConnectedChannels)
import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.DiffusionMode
import Ouroboros.Network.Driver.Simple (runPeer)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux qualified as Mux
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Protocol.Handshake (Acceptable (acceptableVersion),
           HandshakeResult (..), codecHandshake, combineVersions,
           simpleSingletonVersions)
import Ouroboros.Network.Protocol.Handshake.Client (handshakeClientPeer)
import Ouroboros.Network.Protocol.Handshake.Server (handshakeServerPeer)

import DMQ.NodeToNode
import Test.DMQ.NodeToNode.SigSubmission qualified as SigSubmission

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "DMQ.Network.NodeToNode"
    [ testProperty "nodeToNodeVersionCodec"  prop_nodeToNodeVersionCodec
    , testProperty "nodeToNodeCodecCBORTerm" prop_nodeToNodeCodec
    , testProperty "mini-protocol ingress limits" prop_nodeToNodeIngressLimits
    , testProperty "negotiates the highest common version" prop_handshake
    , SigSubmission.tests
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


prop_nodeToNodeVersionCodec :: Property
prop_nodeToNodeVersionCodec = once $ conjoin
    [ counterexample (show version) $ conjoin
        [ encodeTerm version
          ===
          CBOR.TInt tag

        , decodeTerm (CBOR.TInt tag)
          ===
          Right version

        , Aeson.toJSON version
          ===
          Aeson.toJSON tag
        ]
    | version <- [minBound..maxBound]
    , let tag = versionTag version
    ]
  where
    versionTag :: NodeToNodeVersion -> Int
    versionTag NodeToNodeV_1 = 1
    versionTag NodeToNodeV_2 = 2
    versionTag NodeToNodeV_3 = 3

    CodecCBORTerm { encodeTerm, decodeTerm } = nodeToNodeVersionCodec


prop_nodeToNodeCodec :: NodeToNodeVersion -> NodeToNodeVersionData -> Bool
prop_nodeToNodeCodec ntnVersion ntnData =
      decodeTerm (encodeTerm ntnData) == Right ntnData
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
      NodeToNodeV_3 -> (11, 101_640, 72_088)

    bundle = nodeToNodeProtocols dmqLimitsAndTimeouts (Protocols run run run) version versionData
    run = Mux.InitiatorAndResponderProtocol peer peer
    peer = Mux.MiniProtocolCb $ \_ _ -> pure ((), Nothing) :: IO ((), Maybe ())

prop_handshake :: Property
prop_handshake = once $ conjoin
    [ counterexample (show (clientMax, serverMax)) $
        let expected = Just (clientMax `min` serverMax) in
        runSimOrThrow (negotiate clientMax serverMax)
        ===
        (expected, expected)
    | clientMax <- [minBound .. maxBound]
    , serverMax <- [minBound .. maxBound]
    ]
  where
    negotiate :: NodeToNodeVersion -> NodeToNodeVersion
              -> IOSim s (Maybe NodeToNodeVersion, Maybe NodeToNodeVersion)
    negotiate clientMax serverMax = do
      (client, server) <- createConnectedChannels
      let dataCodec = mkVersionedCodecCBORTerm nodeToNodeCodecCBORTerm
      (clientResult, serverResult) <- concurrently
        (runPeer nullTracer (codec clientMax) client $
           handshakeClientPeer dataCodec acceptableVersion (versions clientMax))
        (runPeer nullTracer (codec serverMax) server $
           handshakeServerPeer dataCodec acceptableVersion query (versions serverMax))
      pure (agreedVersion clientResult, agreedVersion serverResult)

    agreedVersion (Right (HandshakeNegotiationResult _ v _), _) = Just v
    agreedVersion _                                             = Nothing

    versions highest = combineVersions
      [ simpleSingletonVersions v versionData (const ())
      | v <- [minBound .. highest] ]
    versionData = stdVersionDataNTN (NetworkMagic 42) InitiatorAndResponderDiffusionMode
                                    PeerSharingDisabled

    -- A legacy decoder also sees the unknown tags in a newer peer's offer.
    codec highest = codecHandshake nodeToNodeVersionCodec {
        decodeTerm = \term -> do
          v <- decodeTerm nodeToNodeVersionCodec term
          if v <= highest then Right v else Left ("unknown version", Nothing)
      }
