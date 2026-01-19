module DMQ.Diffusion.PeerSelection where

import Control.Concurrent.Class.MonadSTM.Strict
import Data.List (sortOn, unfoldr)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word32)
import Ouroboros.Network.PeerSelection
import System.Random (Random (..), StdGen, split)

-- | Trivial peer selection policy used as dummy value
--
policy :: forall peerAddr m.
          ( MonadSTM m
          , Ord peerAddr
          ) 
       => StrictTVar m StdGen
       -> PeerSelectionPolicy peerAddr m
policy rngVar =
  PeerSelectionPolicy {
    policyPickKnownPeersForPeerShare = simplePromotionPolicy,
    policyPickColdPeersToPromote     = simplePromotionPolicy,
    policyPickWarmPeersToPromote     = simplePromotionPolicy,
    policyPickInboundPeers           = simplePromotionPolicy,

    policyPickHotPeersToDemote       = hotDemotionPolicy,
    policyPickWarmPeersToDemote      = warmDemotionPolicy,
    policyPickColdPeersToForget      = coldForgetPolicy,

    policyFindPublicRootTimeout      = 5,
    policyMaxInProgressPeerShareReqs = 0,
    policyPeerShareRetryTime         = 0, -- seconds
    policyPeerShareBatchWaitTime     = 0, -- seconds
    policyPeerShareOverallTimeout    = 0, -- seconds
    policyPeerShareActivationDelay   = 2 -- seconds
  }
  where
    hotDemotionPolicy :: PickPolicy peerAddr (STM m)
    hotDemotionPolicy _ _ _ available pickNum = do
      available' <- addRand rngVar available (,)
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn snd
             . Map.assocs
             $ available'

    -- Randomly pick peers to demote, peers with knownPeerTepid set are twice
    -- as likely to be demoted.
    warmDemotionPolicy :: PickPolicy peerAddr (STM m)
    warmDemotionPolicy _ _ isTepid available pickNum = do
      available' <- addRand rngVar available (tepidWeight isTepid)
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn snd
             . Map.assocs
             $ available'

    simplePromotionPolicy :: PickPolicy peerAddr (STM m)
    simplePromotionPolicy _ _ _ available pickNum = do
      available' <- addRand rngVar available (,)
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn snd
             . Map.assocs
             $ available'

    -- Randomly pick peers to forget, peers with failures are more likely to
    -- be forgotten.
    coldForgetPolicy :: PickPolicy peerAddr (STM m)
    coldForgetPolicy _ failCnt _ available pickNum = do
      available' <- addRand rngVar available (failWeight failCnt)
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn snd
             . Map.assocs
             $ available'

    -- Failures lowers r
    failWeight :: (peerAddr -> Int)
                -> peerAddr
                -> Word32
                -> (peerAddr, Word32)
    failWeight failCnt peer r =
        (peer, r `div` fromIntegral (failCnt peer + 1))

    -- Tepid flag cuts r in half
    tepidWeight :: (peerAddr -> Bool)
                -> peerAddr
                -> Word32
                -> (peerAddr, Word32)
    tepidWeight isTepid peer r =
          if isTepid peer then (peer, r `div` 2)
                          else (peer, r)


 -- Add scaled random number in order to prevent ordering based on SockAddr
addRand :: ( MonadSTM m
           , Ord peerAddr
           )
        => StrictTVar m StdGen
        -> Set.Set peerAddr
        -> (peerAddr -> Word32 -> (peerAddr, Word32))
        -> STM m (Map.Map peerAddr Word32)
addRand rngVar available scaleFn = do
  inRng <- readTVar rngVar

  let (rng, rng') = split inRng
      rns = take (Set.size available) $ unfoldr (Just . random)  rng :: [Word32]
      available' = Map.fromList $ zipWith scaleFn (Set.toList available) rns
  writeTVar rngVar rng'
  return available'

