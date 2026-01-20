{-# LANGUAGE ScopedTypeVariables #-}

module Test.DMQ.SigSubmission.SigLogic
  ( ArbSigDecisionPolicy (..)
  , SigSubmissionState (..)
  , PeerAddr
  ) where

import Prelude hiding (seq)

import Data.Function (on)
import Data.List (nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Ouroboros.Network.Protocol.TxSubmission2.Type

import DMQ.Protocol.SigSubmissionV2.Type (NumIdsReq(..))
import DMQ.SigSubmission.Inbound.Policy (SigDecisionPolicy (..))

import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.Ouroboros.Network.Utils (SmallDelay)
import Test.DMQ.SigSubmission.Types (Sig (..))

type PeerAddr = Int

newtype ArbSigDecisionPolicy = ArbSigDecisionPolicy SigDecisionPolicy
  deriving Show

instance Arbitrary ArbSigDecisionPolicy where
    arbitrary =
          ArbSigDecisionPolicy . fixupSigDecisionPolicy
      <$> ( SigDecisionPolicy
            <$> (getSmall . getPositive <$> arbitrary)
            <*> (getSmall . getPositive <$> arbitrary)
            <*> (SizeInBytes . getPositive <$> arbitrary)
            <*> (SizeInBytes . getPositive <$> arbitrary)
            <*> (getSmall . getPositive <$> arbitrary)
            <*> (realToFrac <$> choose (0 :: Double, 2))
            <*> choose (0, 1)
            <*> choose (0, 1800))

    shrink (ArbSigDecisionPolicy a@SigDecisionPolicy {
              maxNumSigIdsToRequest,
              sigsSizeInflightPerPeer,
              maxSigsSizeInflight,
              sigInflightMultiplicity }) =
      [ ArbSigDecisionPolicy a { maxNumSigIdsToRequest = NumIdsReq x }
      | (Positive (Small x)) <- shrink (Positive (Small (getNumIdsReq maxNumSigIdsToRequest)))
      ]
      ++
      [ ArbSigDecisionPolicy . fixupSigDecisionPolicy
      $ a { sigsSizeInflightPerPeer = SizeInBytes s }
      | Positive s <- shrink (Positive (getSizeInBytes sigsSizeInflightPerPeer))
      ]
      ++
      [ ArbSigDecisionPolicy . fixupSigDecisionPolicy
      $ a { maxSigsSizeInflight = SizeInBytes s }
      | Positive s <- shrink (Positive (getSizeInBytes maxSigsSizeInflight))
      ]
      ++
      [ ArbSigDecisionPolicy . fixupSigDecisionPolicy
      $ a { sigInflightMultiplicity = x }
      | Positive (Small x) <- shrink (Positive (Small sigInflightMultiplicity))
      ]

fixupSigDecisionPolicy :: SigDecisionPolicy -> SigDecisionPolicy
fixupSigDecisionPolicy a@SigDecisionPolicy { sigsSizeInflightPerPeer,
                                             maxSigsSizeInflight }
 = a { sigsSizeInflightPerPeer = sigsSizeInflightPerPeer',
       maxSigsSizeInflight     = maxSigsSizeInflight' }
 where
   sigsSizeInflightPerPeer' = min sigsSizeInflightPerPeer maxSigsSizeInflight
   maxSigsSizeInflight'     = max sigsSizeInflightPerPeer maxSigsSizeInflight



data SigSubmissionState =
  SigSubmissionState {
      peerMap :: Map Int ( [Sig Int]
                         , Maybe (Positive SmallDelay)
                         , Maybe (Positive SmallDelay)
                         -- ^ The delay must be smaller (<) than 5s, so that overall
                         -- delay is less than 10s, otherwise 'smallDelay' in
                         -- 'timeLimitsSigSubmission2' will kick in.
                         )
    , decisionPolicy :: SigDecisionPolicy
  }
  deriving (Show)

instance Arbitrary SigSubmissionState where
  arbitrary = do
    ArbSigDecisionPolicy decisionPolicy <- arbitrary
    peersN <- choose (1, 10)
    sigsN <- choose (1, 10)
    -- NOTE: using sortOn would forces sig-decision logic to download sigs in the
    -- order of unacknowledgedSigids.  This could be useful to get better
    -- properties when wrongly sized sigs are present.
    sigs <- divvy sigsN . nubBy (on (==) getSigId) {- . List.sortOn getSigid -} <$> vectorOf (peersN * sigsN) arbitrary
    peers <- vectorOf peersN arbitrary
    peersState <- zipWith (curry (\(a, (b, c)) -> (a, b, c))) sigs
              <$> vectorOf peersN arbitrary
    return SigSubmissionState  { peerMap = Map.fromList (zip peers peersState),
                                decisionPolicy
                              }
  shrink SigSubmissionState { peerMap, decisionPolicy } =
    SigSubmissionState <$> shrinkMap1 peerMap
                      <*> [ policy
                          | ArbSigDecisionPolicy policy <- shrink (ArbSigDecisionPolicy decisionPolicy)
                          ]
    where
      shrinkMap1 :: Ord k => Map k v -> [Map k v]
      shrinkMap1 m
        | Map.size m <= 1 = [m]
        | otherwise       = [Map.delete k m | k <- Map.keys m] ++ singletonMaps
        where
          singletonMaps = [Map.singleton k v | (k, v) <- Map.toList m]



-- | Split a list into sub list of at most `n` elements.
--
divvy :: Int -> [a] -> [[a]]
divvy _ [] = []
divvy n as = take n as : divvy n (drop n as)

