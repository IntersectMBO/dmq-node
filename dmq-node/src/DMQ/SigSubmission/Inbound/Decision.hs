{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module DMQ.SigSubmission.Inbound.Decision
  ( SigDecision (..)
  , emptySigDecision
    -- * Internal API exposed for testing
  , makeDecisions
  , filterActivePeers
  , pickSigsToDownload
  ) where

import Control.Arrow ((>>>))
import Control.Exception (assert)

import Data.Bifunctor (second)
import Data.Hashable
import Data.List qualified as List
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Random (random)

import Data.Sequence.Strict qualified as StrictSeq
import Ouroboros.Network.Protocol.TxSubmission2.Type

import DMQ.SigSubmission.Inbound.Policy
import DMQ.SigSubmission.Inbound.State
import DMQ.SigSubmission.Inbound.Types


-- | Make download decisions.
--
makeDecisions
    :: forall peeraddr sigid sig.
       ( Ord peeraddr
       , Ord sigid
       , Hashable peeraddr
       )
    => SigDecisionPolicy
    -- ^ decision policy
    -> SharedSigState peeraddr sigid sig
    -- ^ decision context
    -> Map peeraddr (PeerSigState sigid sig)
    -- ^ list of available peers.
    --
    -- This is a subset of `peerSigStates` of peers which either:
    -- * can be used to download a `sig`,
    -- * can acknowledge some `sigid`s.
    --
    -> ( SharedSigState peeraddr sigid sig
       , Map peeraddr (SigDecision sigid sig)
       )
makeDecisions policy st =
    let (salt, rng') = random (peerRng st)
        st' = st { peerRng = rng' }
    in  fn
      . pickSigsToDownload policy st'
      . orderByRejections salt
  where
    fn :: forall a.
          (a, [(peeraddr, SigDecision sigid sig)])
       -> (a, Map peeraddr (SigDecision sigid sig))
    fn (a, as) = (a, Map.fromList as)


-- | Order peers by how useful the sigs they have provided are.
--
-- sigs delivered late will fail to apply because they were included in
-- a recently adopted block. Peers can race against each other by setting
-- `sigInflightMultiplicity` to > 1. In case of a tie a hash of the peeraddr
-- is used as a tie breaker. Since every invocation use a new salt a given
-- peeraddr does not have an advantage over time.
--
orderByRejections :: Hashable peeraddr
                  => Int
                  -> Map peeraddr (PeerSigState sigid sig)
                  -> [(peeraddr, PeerSigState sigid sig)]
orderByRejections salt =
        List.sortOn (\(peeraddr, ps) -> (score ps, hashWithSalt salt peeraddr))
      . Map.toList


-- | Internal state of `pickSigsToDownload` computation.
--
data St peeraddr sigid sig =
   St { stInflightSize             :: !SizeInBytes,
        -- ^ size of all `sig`s in-flight.

        stInflight                 :: !(Map sigid Int),
        -- ^ `sigid`s in-flight.

        stAcknowledged             :: !(Map sigid Int),
        -- ^ acknowledged `sigid` with multiplicities.  It is used to update
        -- `referenceCounts`.

        stInSubmissionToMempoolTxs :: !(Set sigid)
        -- ^ sigs on their way to the mempool. Used to prevent issueing new
        -- fetch requests for them.
      }


-- | Distribute `sig`'s to download among available peers.  Peers are considered
-- in the given order.
--
-- * pick sigs from the set of available sig's (in `sigid` order, note these sets
--   might be different for different peers).
-- * pick sigs until the peers in-flight limit (we can go over the limit by one sig)
--   (`sigsSizeInflightPerPeer` limit)
-- * pick sigs until the overall in-flight limit (we can go over the limit by one sig)
--   (`maxSigsSizeInflight` limit)
-- * each sig can be downloaded simultaneously from at most
--   `sigInflightMultiplicity` peers.
--
pickSigsToDownload
  :: forall peeraddr sigid sig.
     ( Ord peeraddr
     , Ord sigid
     )
  => SigDecisionPolicy
  -- ^ decision policy
  -> SharedSigState peeraddr sigid sig
  -- ^ shared state

  -> [(peeraddr, PeerSigState sigid sig)]
  -> ( SharedSigState peeraddr sigid sig 
     , [(peeraddr, SigDecision sigid sig)]
     )

pickSigsToDownload policy@SigDecisionPolicy { sigsSizeInflightPerPeer,
                                             maxSigsSizeInflight,
                                             sigInflightMultiplicity }
                  sharedState@SharedSigState { peerSigStates,
                                              inflightSigs,
                                              inflightSigsSize,
                                              bufferedSigs,
                                              inSubmissionToMempoolSigs,
                                              referenceCounts } =
    -- outer fold: fold `[(peeraddr, PeerSigState sigid sig)]`
    List.mapAccumR
      accumFn
      -- initial state
      St { stInflight                 = inflightSigs,
           stInflightSize             = inflightSigsSize,
           stAcknowledged             = Map.empty,
           stInSubmissionToMempoolTxs = Map.keysSet inSubmissionToMempoolSigs }

    >>>
      gn
  where
    accumFn :: St peeraddr sigid sig
            -> (peeraddr, PeerSigState sigid sig)
            -> ( St peeraddr sigid sig
               , ( (peeraddr, PeerSigState sigid sig)
                 , SigDecision sigid sig
                 )
               )
    accumFn
      st@St { stInflight,
              stInflightSize,
              stAcknowledged,
              stInSubmissionToMempoolTxs }
      ( peeraddr
      , peerSigState@PeerSigState { availableSigIds,
                                    unknownSigs,
                                    requestedSigsInflight,
                                    requestedSigsInflightSize
                                  }
      )
      =
      let sizeInflightAll   :: SizeInBytes
          sizeInflightOther :: SizeInBytes

          sizeInflightAll   = stInflightSize
          sizeInflightOther = sizeInflightAll - requestedSigsInflightSize

      in if sizeInflightAll >= maxSigsSizeInflight
        then let ( numIdsAck
                   , numIdsReq
                   , txsToMempool@TxsToMempool { listOfTxsToMempool }
                   , RefCountDiff { txIdsToAck }
                   , peerSigState'
                   ) = acknowledgeSigIds policy sharedState peerSigState

                 stAcknowledged' = Map.unionWith (+) stAcknowledged txIdsToAck
                 stInSubmissionToMempoolTxs' = stInSubmissionToMempoolTxs
                                            <> Set.fromList (map fst listOfTxsToMempool)
             in
             if requestedSigIdsInflight peerSigState' > 0
               then
                 -- we have sigids to request
                 ( st { stAcknowledged             = stAcknowledged'
                      , stInSubmissionToMempoolTxs = stInSubmissionToMempoolTxs' }
                 , ( (peeraddr, peerSigState')
                     , SigDecision { sigdSigIdsToAcknowledge = numIdsAck,
                                     sigdSigIdsToRequest     = numIdsReq,
                                     sigdPipelineSigIds      = not
                                                           . StrictSeq.null
                                                           . unacknowledgedSigIds
                                                           $ peerSigState',
                                     sigdSigsToRequest       = Map.empty,
                                     sigdSigsToMempool       = txsToMempool
                                   }
                     )
                 )
               else
                 -- there are no `sigid`s to request, nor we can request `sig`s due
                 -- to in-flight size limits
                 ( st
                 , ( (peeraddr, peerSigState')
                   , emptySigDecision
                   )
                 )
        else
          let requestedSigsInflightSize' :: SizeInBytes
              sigsToRequestMap :: Map sigid SizeInBytes

              (requestedSigsInflightSize', sigsToRequestMap) =
                -- inner fold: fold available `sigid`s
                --
                -- Note: although `Map.foldrWithKey` could be used here, it
                -- does not allow to short circuit the fold, unlike
                -- `foldWithState`.
                foldWithState
                  (\(sigid, (sigSize, inflightMultiplicity)) sizeInflight ->
                    if -- note that we pick `sigid`'s as long the `s` is
                       -- smaller or equal to `sigsSizeInflightPerPeer`.
                       sizeInflight <= sigsSizeInflightPerPeer
                       -- overall `sig`'s in-flight must be smaller than
                       -- `maxSigsSizeInflight`
                    && sizeInflight + sizeInflightOther <= maxSigsSizeInflight
                       -- the signature must not be downloaded from more
                       -- than `sigInflightMultiplicity` peers simultaneously
                    && inflightMultiplicity < sigInflightMultiplicity
                    -- TODO: we must validate that `sigSize` is smaller than
                    -- maximum sigs size
                    then Just (sizeInflight + sigSize, (sigid, sigSize))
                    else Nothing
                  )
                  (Map.assocs $
                    -- merge `availableSigIds` with `stInflight`, so we don't
                    -- need to lookup into `stInflight` on every `sigid` which
                    -- is in `availableSigIds`.
                    Map.merge (Map.mapMaybeMissing \_sigid -> Just . (,0))
                               Map.dropMissing
                              (Map.zipWithMatched \_sigid -> (,))

                              availableSigIds
                              stInflight
                    -- remove `sig`s which were already downloaded by some
                    -- other peer or are in-flight or unknown by this peer.
                    `Map.withoutKeys` (
                         Map.keysSet bufferedSigs
                      <> requestedSigsInflight
                      <> unknownSigs
                      <> stInSubmissionToMempoolTxs
                    )
                  )
                  requestedSigsInflightSize
                  -- pick from `sigid`'s which are available from that given
                  -- peer.  Since we are folding a dictionary each `sigid`
                  -- will be selected only once from a given peer (at least
                  -- in each round).

              sigsToRequest = Map.keysSet sigsToRequestMap
              peerSigState' = peerSigState {
                  requestedSigsInflightSize = requestedSigsInflightSize',
                  requestedSigsInflight     = requestedSigsInflight
                                           <> sigsToRequest
                }

              ( numIdsAck
                , numIdsReq
                , txsToMempool@TxsToMempool { listOfTxsToMempool }
                , RefCountDiff { txIdsToAck }
                , peerSigState''
                ) = acknowledgeSigIds policy sharedState peerSigState'

              stAcknowledged' = Map.unionWith (+) stAcknowledged txIdsToAck

              stInflightDelta :: Map sigid Int
              stInflightDelta = Map.fromSet (\_ -> 1) sigsToRequest
                                -- note: this is right since every `sigid`
                                -- could be picked at most once

              stInflight' :: Map sigid Int
              stInflight' = Map.unionWith (+) stInflightDelta stInflight

              stInSubmissionToMempoolTxs' = stInSubmissionToMempoolTxs
                                         <> Set.fromList (map fst listOfTxsToMempool)
          in
            if requestedSigIdsInflight peerSigState'' > 0
              then
                -- we can request `sigid`s & `sig`s
                ( St { stInflight                 = stInflight',
                       stInflightSize             = sizeInflightOther + requestedSigsInflightSize',
                       stAcknowledged             = stAcknowledged',
                       stInSubmissionToMempoolTxs = stInSubmissionToMempoolTxs' }
                , ( (peeraddr, peerSigState'')
                  , SigDecision { sigdSigIdsToAcknowledge = numIdsAck,
                                  sigdPipelineSigIds      = not
                                                        . StrictSeq.null
                                                        . unacknowledgedSigIds
                                                        $ peerSigState'',
                                  sigdSigIdsToRequest     = numIdsReq,
                                  sigdSigsToRequest       = sigsToRequestMap,
                                  sigdSigsToMempool       = txsToMempool
                                }
                  )
                )
              else
                -- there are no `sigid`s to request, only `sig`s.
                ( st { stInflight                 = stInflight',
                       stInflightSize             = sizeInflightOther + requestedSigsInflightSize',
                       stInSubmissionToMempoolTxs = stInSubmissionToMempoolTxs'
                     }
                , ( (peeraddr, peerSigState'')
                  , emptySigDecision { sigdSigsToRequest = sigsToRequestMap }
                  )
                )

    gn :: ( St peeraddr sigid sig
          , [((peeraddr, PeerSigState sigid sig), SigDecision sigid sig)]
          )
       -> ( SharedSigState peeraddr sigid sig
          , [(peeraddr, SigDecision sigid sig)]
          )
    gn
      ( St { stInflight,
             stInflightSize,
             stAcknowledged }
      , as
      )
      =
      let peerSigStates' = Map.fromList ((\(a,_) -> a) <$> as)
                       <> peerSigStates

          referenceCounts' =
            Map.merge (Map.mapMaybeMissing \_ x -> Just x)
                      (Map.mapMaybeMissing \_ _ -> assert False Nothing)
                      (Map.zipWithMaybeMatched \_ x y -> if x > y then Just $! x - y
                                                                  else Nothing)
                      referenceCounts
                      stAcknowledged

          liveSet = Map.keysSet referenceCounts'

          bufferedSigs' = bufferedSigs
                         `Map.restrictKeys`
                         liveSet

          inSubmissionToMempoolSigs' =
            List.foldl' updateInSubmissionToMempoolSigs inSubmissionToMempoolSigs as

      in ( sharedState {
             peerSigStates             = peerSigStates',
             inflightSigs              = stInflight,
             inflightSigsSize          = stInflightSize,
             bufferedSigs              = bufferedSigs',
             referenceCounts           = referenceCounts',
             inSubmissionToMempoolSigs = inSubmissionToMempoolSigs'}
         , -- exclude empty results
           mapMaybe (\((a, _), b) -> case b of
                      SigDecision { sigdSigIdsToAcknowledge = 0,
                                    sigdSigIdsToRequest     = 0,
                                    sigdSigsToRequest,
                                    sigdSigsToMempool = TxsToMempool { listOfTxsToMempool } }
                                 | null sigdSigsToRequest
                                 , null listOfTxsToMempool
                                 -> Nothing
                      _          -> Just (a, b)
                    )
                    as
         )

      where
        updateInSubmissionToMempoolSigs
          :: forall a.
             Map sigid Int
          -> (a, SigDecision sigid sig)
          -> Map sigid Int
        updateInSubmissionToMempoolSigs m (_,SigDecision { sigdSigsToMempool } ) =
            List.foldl' fn m (listOfTxsToMempool sigdSigsToMempool)
          where
            fn :: Map sigid Int
               -> (sigid,sig)
               -> Map sigid Int
            fn x (sigid,_) = Map.alter (\case Nothing -> Just 1
                                              Just n  -> Just $! succ n) sigid x


-- | Filter peers which can either download a `sig` or acknowledge `sigid`s.
--
filterActivePeers
    :: forall peeraddr sigid sig.
       Ord sigid
    => SigDecisionPolicy
    -> SharedSigState peeraddr sigid sig
    -> Map peeraddr (PeerSigState sigid sig)
filterActivePeers
    policy@SigDecisionPolicy {
      maxUnacknowledgedSigIds,
      sigsSizeInflightPerPeer,
      maxSigsSizeInflight,
      sigInflightMultiplicity
    }
    sharedSigState@SharedSigState {
      peerSigStates,
      bufferedSigs,
      inflightSigs,
      inflightSigsSize,
      inSubmissionToMempoolSigs
    }
    | inflightSigsSize > maxSigsSizeInflight
      -- we might be able to request sigids, we cannot download sigs
    = Map.filter fn peerSigStates
    | otherwise
      -- we might be able to request sigids or sigs.
    = Map.filter gn peerSigStates
  where
    unrequestable = Map.keysSet (Map.filter (>= sigInflightMultiplicity) inflightSigs)
                 <> Map.keysSet bufferedSigs

    fn :: PeerSigState sigid sig -> Bool
    fn peerSigState@PeerSigState {
        requestedSigIdsInflight
       } =
           requestedSigIdsInflight == 0
           -- if a peer has sigids in-flight, we cannot request more sigids or sigs.
        && requestedSigIdsInflight + numOfUnacked <= maxUnacknowledgedSigIds
        && sigIdsToRequest > 0
      where
        -- Split `unacknowledgedSigIds'` into the longest prefix of `sigid`s which
        -- can be acknowledged and the unacknowledged `sigid`s.
        (sigIdsToRequest, _, unackedSigIds) = splitAcknowledgedSigIds policy sharedSigState peerSigState
        numOfUnacked = fromIntegral (StrictSeq.length unackedSigIds)

    gn :: PeerSigState sigid sig -> Bool
    gn peerSigState@PeerSigState { unacknowledgedSigIds,
                                  requestedSigIdsInflight,
                                  requestedSigsInflight,
                                  requestedSigsInflightSize,
                                  availableSigIds,
                                  unknownSigs
                                } =
          (    requestedSigIdsInflight == 0
            && requestedSigIdsInflight + numOfUnacked <= maxUnacknowledgedSigIds
            && sigIdsToRequest > 0
          )
        || (underSizeLimit && not (Map.null downloadable))
      where
        numOfUnacked   = fromIntegral (StrictSeq.length unacknowledgedSigIds)
        underSizeLimit = requestedSigsInflightSize <= sigsSizeInflightPerPeer
        downloadable   = availableSigIds
            `Map.withoutKeys` requestedSigsInflight
            `Map.withoutKeys` unknownSigs
            `Map.withoutKeys` unrequestable
            `Map.withoutKeys` Map.keysSet inSubmissionToMempoolSigs

        -- Split `unacknowledgedTxIds'` into the longest prefix of `sigid`s which
        -- can be acknowledged and the unacknowledged `sigid`s.
        (sigIdsToRequest, _, _) = splitAcknowledgedSigIds policy sharedSigState peerSigState

--
-- Auxiliary functions
--

-- | A fold with state implemented as a `foldr` to take advantage of fold-build
-- fusion optimisation.
--
foldWithState
  :: forall s a b c.
     Ord b
  => (a -> s -> Maybe (s, (b, c)))
  -> [a] -> s -> (s, Map b c)
{-# INLINE foldWithState #-}

foldWithState f = foldr cons nil
  where
    cons :: a
         -> (s -> (s, Map b c))
         -> (s -> (s, Map b c))
    cons a k = \ !s ->
      case f a s of
        Nothing -> nil s
        Just (!s', (!b, !c)) ->
          case Map.insert b c `second` k s' of
            r@(!_s, !_bs) -> r

    nil :: s -> (s, Map b c)
    nil = \ !s -> (s, Map.empty)
