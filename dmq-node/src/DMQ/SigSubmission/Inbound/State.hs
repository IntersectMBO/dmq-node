{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.SigSubmission.Inbound.State
  ( -- * Core API
    SharedSigState (..)
  , PeerSigState (..)
  , SharedSigStateVar
  , newSharedSigStateVar
  , receivedSigIds
  , collectSigs
  , acknowledgeSigIds
  , splitAcknowledgedSigIds
  , tickTimedSigs
  , const_MAX_SIG_SIZE_DISCREPENCY
    -- * Internals, only exported for testing purposes:
  , RefCountDiff (..)
  , updateRefCounts
  , receivedSigIdsImpl
  , collectSigsImpl
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer, traceWith)

import Data.Foldable (fold, toList)
import Data.Foldable qualified as Foldable
import Data.Functor (($>))
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import System.Random (StdGen)

import Ouroboros.Network.SizeInBytes (SizeInBytes (..))
import Ouroboros.Network.TxSubmission.Mempool.Reader (MempoolSnapshot (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.State (RefCountDiff (..),
                   updateRefCounts)

import DMQ.Protocol.SigSubmissionV2.Type (NumIdsAck)

import DMQ.SigSubmission.Inbound.Policy
import DMQ.SigSubmission.Inbound.Types


--
-- Pure public API
--

acknowledgeSigIds
    :: forall peeraddr sig sigid.
       Ord sigid
    => SigDecisionPolicy
    -> SharedSigState peeraddr sigid sig
    -> PeerSigState sigid sig
    -> ( NumIdsAck
       , NumIdsReq
       , TxsToMempool sigid sig
       , RefCountDiff sigid
       , PeerSigState sigid sig
       )
    -- ^ number of sigid to acknowledge, requests, sigs which we can submit to the
    -- mempool, sigids to acknowledge with multiplicities, updated PeerSigState.
{-# INLINE acknowledgeSigIds #-}

acknowledgeSigIds
    policy
    sharedSigState
    ps@PeerSigState { availableSigIds,
                      unknownSigs,
                      requestedSigIdsInflight,
                      downloadedSigs,
                      score,
                      toMempoolSigs
                   }
    =
    -- We can only acknowledge sigids when we can request new ones, since
    -- a `MsgRequestSigIds` for 0 sigids is a protocol error.
    if sigIdsToRequest > 0
      then
      ( sigIdsToAcknowledge
      , sigIdsToRequest
      , TxsToMempool sigsToMempool
      , refCountDiff
      , ps { unacknowledgedSigIds    = unacknowledgedSigIds',
             availableSigIds         = availableSigIds',
             unknownSigs             = unkownSigs',
             requestedSigIdsInflight = requestedSigIdsInflight
                                     + sigIdsToRequest,
             downloadedSigs          = downloadedSigs',
             score                   = score',
             toMempoolSigs           = toMempoolSigs' }
      )
      else
      ( 0
      , 0
      , TxsToMempool sigsToMempool
      , RefCountDiff Map.empty
      , ps { toMempoolSigs = toMempoolSigs' }
      )
  where
    -- Split `unacknowledgedSigIds'` into the longest prefix of `sigid`s which
    -- can be acknowledged and the unacknowledged `sigid`s.
    (sigIdsToRequest, acknowledgedSigIds, unacknowledgedSigIds')
      = splitAcknowledgedSigIds policy sharedSigState ps

    sigsToMempool = [ (sigid, sig)
                   | sigid <- toList toMempoolSigIds
                   , sigid `Map.notMember` bufferedSigs sharedSigState
                   , sig <- maybeToList $ sigid `Map.lookup` downloadedSigs
                   ]
    (toMempoolSigIds, _) =
      StrictSeq.spanl (`Map.member` downloadedSigs) acknowledgedSigIds


    sigsToMempoolMap = Map.fromList sigsToMempool

    toMempoolSigs' = toMempoolSigs <> sigsToMempoolMap

    (downloadedSigs', ackedDownloadedSigs) = Map.partitionWithKey (\sigid _ -> sigid `Set.member` liveSet) downloadedSigs
    -- lateSigs: sigs which were downloaded by another peer before we
    -- downloaded them; it relies on that `txToMempool` filters out
    -- `bufferedSigs`.
    lateSigs = Map.filterWithKey (\sigid _ -> sigid `Map.notMember` sigsToMempoolMap) ackedDownloadedSigs
    score' = score + fromIntegral (Map.size lateSigs)

    -- the set of live `sigids`
    liveSet  = Set.fromList (toList unacknowledgedSigIds')

    availableSigIds' = availableSigIds
                      `Map.restrictKeys`
                      liveSet

    -- We remove all acknowledged `sigid`s which are not in
    -- `unacknowledgedSigIds''`, but also return the unknown set before any
    -- modifications (which is used to compute `unacknowledgedSigIds''`
    -- above).
    unkownSigs' = unknownSigs `Set.intersection` liveSet

    refCountDiff = RefCountDiff
                 $ foldr (Map.alter fn)
                         Map.empty acknowledgedSigIds
      where
        fn :: Maybe Int -> Maybe Int
        fn Nothing  = Just 1
        fn (Just n) = Just $! n + 1

    sigIdsToAcknowledge :: NumIdsAck
    sigIdsToAcknowledge = fromIntegral $ StrictSeq.length acknowledgedSigIds


-- | Split unacknowledged sigids into acknowledged and unacknowledged parts, also
-- return number of sigids which can be requested.
--
splitAcknowledgedSigIds
  :: Ord sigid
  => SigDecisionPolicy
  -> SharedSigState peer sigid sig
  -> PeerSigState  sigid sig
  -> (NumIdsReq, StrictSeq.StrictSeq sigid, StrictSeq.StrictSeq sigid)
  -- ^ number of sigids to request, acknowledged sigids, unacknowledged sigids
splitAcknowledgedSigIds
    SigDecisionPolicy {
      maxUnacknowledgedSigIds,
      maxNumSigIdsToRequest
    }
    SharedSigState {
      bufferedSigs
    }
    PeerSigState {
      unacknowledgedSigIds,
      unknownSigs,
      downloadedSigs,
      requestedSigsInflight,
      requestedSigIdsInflight
    }
    =
    (sigIdsToRequest, acknowledgedSigIds', unacknowledgedSigIds')
  where
    (acknowledgedSigIds', unacknowledgedSigIds')
      = StrictSeq.spanl (\sigid ->
                            sigid `Set.notMember` requestedSigsInflight
                         && (
                              sigid `Map.member` downloadedSigs
                           || sigid `Set.member` unknownSigs
                           || sigid `Map.member` bufferedSigs
                         )
                        )
                        unacknowledgedSigIds

    numOfUnacked        = StrictSeq.length unacknowledgedSigIds
    numOfAcked          = StrictSeq.length acknowledgedSigIds'
    unackedAndRequested = fromIntegral numOfUnacked + requestedSigIdsInflight

    sigIdsToRequest =
        assert (unackedAndRequested <= maxUnacknowledgedSigIds) $
        assert (requestedSigIdsInflight <= maxNumSigIdsToRequest) $
        (maxUnacknowledgedSigIds - unackedAndRequested + fromIntegral numOfAcked)
        `min`
        (maxNumSigIdsToRequest - requestedSigIdsInflight)


tickTimedSigs :: forall peeraddr sig sigid.
                (Ord sigid)
             => Time
             -> SharedSigState peeraddr sigid sig
             -> SharedSigState peeraddr sigid sig
tickTimedSigs now st@SharedSigState{ timedSigs
                                 , referenceCounts
                                 , bufferedSigs } =
    let (expiredSigs', timedSigs') =
          case Map.splitLookup now timedSigs of
            (expired, Just sigids, timed) ->
              (expired, -- Map.split doesn't include the `now` entry in the map
                        Map.insert now sigids timed)
            (expired, Nothing, timed) ->
              (expired, timed)
        refDiff = Map.foldl' fn Map.empty expiredSigs'
        referenceCounts' = updateRefCounts referenceCounts (RefCountDiff refDiff)
        liveSet = Map.keysSet referenceCounts'
        bufferedSigs' = bufferedSigs `Map.restrictKeys` liveSet in
    st { timedSigs        = timedSigs'
       , referenceCounts = referenceCounts'
       , bufferedSigs     = bufferedSigs'
       }
  where
    fn :: Map sigid Int
       -> [sigid]
       -> Map sigid Int
    fn m sigids = Foldable.foldl' gn m sigids

    gn :: Map sigid Int
       -> sigid
       -> Map sigid Int
    gn m sigid = Map.alter af sigid m

    af :: Maybe Int
       -> Maybe Int
    af Nothing  = Just 1
    af (Just n) = Just $! succ n

--
-- Pure internal API
--

-- | Insert received `sigid`s and return the number of sigids to be acknowledged
-- and the updated `SharedSigState`.
--
receivedSigIdsImpl
    :: forall peeraddr sig sigid.
       (Ord sigid, Ord peeraddr, HasCallStack)
    => (sigid -> Bool)      -- ^ check if sigid is in the mempool, ref
                           -- 'mempoolHasSig'
    -> peeraddr
    -> NumIdsReq
    -- ^ number of requests to subtract from
    -- `requestedSigIdsInflight`

    -> StrictSeq sigid
    -- ^ sequence of received `sigids`
    -> Map sigid SizeInBytes
    -- ^ received `sigid`s with sizes

    -> SharedSigState peeraddr sigid sig
    -> SharedSigState peeraddr sigid sig

receivedSigIdsImpl
    mempoolHasSig
    peeraddr reqNo sigidsSeq sigidsMap
    st@SharedSigState{ peerSigStates,
                      bufferedSigs,
                      referenceCounts }
    =
    -- using `alterF` so the update of `PeerSigState` is done in one lookup
    case Map.alterF (fmap Just . fn . fromJust)
                    peeraddr
                    peerSigStates of
      ( st', peerSigStates' ) ->
        st' { peerSigStates = peerSigStates' }

  where
    -- update `PeerSigState` and return number of `sigid`s to acknowledged and
    -- updated `SharedSigState`.
    fn :: PeerSigState sigid sig
       -> ( SharedSigState peeraddr sigid sig
          , PeerSigState sigid sig
          )
    fn ps@PeerSigState { availableSigIds,
                        requestedSigIdsInflight,
                        unacknowledgedSigIds } =
        (st', ps')
      where
        --
        -- Handle new `sigid`s
        --

        -- Divide the new sigids in two: those that are already in the mempool
        -- and those that are not. We'll request some sigs from the latter.
        (ignoredSigIds, availableSigIdsMap) =
          Map.partitionWithKey
            (\sigid _ -> mempoolHasSig sigid)
            sigidsMap

        -- Add all `sigids` from `availableSigIdsMap` which are not
        -- unacknowledged or already buffered. Unacknowledged sigids must have
        -- already been added to `availableSigIds` map before.
        availableSigIds' =
          Map.foldlWithKey
            (\m sigid sizeInBytes -> Map.insert sigid sizeInBytes m)
            availableSigIds
            (Map.filterWithKey
                (\sigid _ -> sigid `notElem` unacknowledgedSigIds
                          && sigid `Map.notMember` bufferedSigs)
                availableSigIdsMap)

        -- Add received sigids to `unacknowledgedSigIds`.
        unacknowledgedSigIds' = unacknowledgedSigIds <> sigidsSeq

        -- Add ignored `sigs` to buffered ones.
        -- Note: we prefer to keep the `sig` if it's already in `bufferedSigs`.
        bufferedSigs' = bufferedSigs
                    <> Map.map (const Nothing) ignoredSigIds

        referenceCounts' =
          Foldable.foldl'
            (flip $ Map.alter (\case
                                 Nothing  -> Just $! 1
                                 Just cnt -> Just $! succ cnt))
            referenceCounts
            sigidsSeq

        st' = st { bufferedSigs     = bufferedSigs',
                   referenceCounts = referenceCounts' }
        ps' = assert (requestedSigIdsInflight >= reqNo)
              ps { availableSigIds         = availableSigIds',
                   unacknowledgedSigIds    = unacknowledgedSigIds',
                   requestedSigIdsInflight = requestedSigIdsInflight - reqNo }

-- | We check advertised sizes up in a fuzzy way.  The advertised and received
-- sizes need to agree up to `const_MAX_SIG_SIZE_DISCREPENCY`.
--
const_MAX_SIG_SIZE_DISCREPENCY :: SizeInBytes
const_MAX_SIG_SIZE_DISCREPENCY = 32

collectSigsImpl
    :: forall peeraddr sig sigid.
       ( Ord peeraddr
       , Ord sigid
       , Show sigid
       , Typeable sigid
       )
    => (sig -> SizeInBytes) -- ^ compute sig size
    -> peeraddr
    -> Map sigid SizeInBytes -- ^ requested sigids
    -> Map sigid sig          -- ^ received sigs
    -> SharedSigState peeraddr sigid sig
    -> Either TxSubmissionProtocolError
              (SharedSigState peeraddr sigid sig)
    -- ^ Return list of `sigid` which sizes didn't match or a new state.
    -- If one of the `sig` has wrong size, we return an error.  The
    -- mini-protocol will throw, which will clean the state map from this peer.
collectSigsImpl sigSize peeraddr requestedSigIdsMap receivedSigs
                st@SharedSigState { peerSigStates } =

    -- using `alterF` so the update of `PeerSigState` is done in one lookup
    case Map.alterF (fmap Just . fn . fromJust)
                    peeraddr
                    peerSigStates of
      (Right st', peerSigStates') ->
        Right st' { peerSigStates = peerSigStates' }
      (Left e, _) ->
        Left $ ProtocolErrorTxSizeError e

  where
    -- Update `PeerSigState` and partially update `SharedSigState` (except of
    -- `peerSigStates`).
    fn :: PeerSigState sigid sig
       -> ( Either [(sigid, SizeInBytes, SizeInBytes)]
                   (SharedSigState peeraddr sigid sig)
          , PeerSigState sigid sig
          )
    fn ps =
        case wrongSizedSigs of
          [] -> ( Right st'
                , ps''
                )
          _  -> ( Left wrongSizedSigs
                , ps
                )
      where
        wrongSizedSigs :: [(sigid, SizeInBytes, SizeInBytes)]
        wrongSizedSigs =
            map (\(a, (b,c)) -> (a,b,c))
          . Map.toList
          $ Map.merge
              Map.dropMissing
              Map.dropMissing
              (Map.zipWithMaybeMatched \_ receivedSize advertisedSize ->
                if receivedSize `checkSigSize` advertisedSize
                  then Nothing
                  else Just (receivedSize, advertisedSize)
              )
              (sigSize `Map.map` receivedSigs)
              requestedSigIdsMap

        checkSigSize :: SizeInBytes
                    -> SizeInBytes
                    -> Bool
        checkSigSize received advertised
          | received > advertised
          = received - advertised <= const_MAX_SIG_SIZE_DISCREPENCY
          | otherwise
          = advertised - received <= const_MAX_SIG_SIZE_DISCREPENCY

        requestedSigIds = Map.keysSet requestedSigIdsMap
        notReceived    = requestedSigIds Set.\\ Map.keysSet receivedSigs
        downloadedSigs' = downloadedSigs ps <> receivedSigs
        -- Add not received sigs to `unkownSigs` before acknowledging sigids.
        unkownSigs'    = unknownSigs ps <> notReceived

        requestedSigsInflight' =
          assert (requestedSigIds `Set.isSubsetOf` requestedSigsInflight ps) $
          requestedSigsInflight ps  Set.\\ requestedSigIds

        requestedSize = fold $ availableSigIds ps `Map.restrictKeys` requestedSigIds
        requestedSigsInflightSize' =
          assert (requestedSigsInflightSize ps >= requestedSize) $
          requestedSigsInflightSize ps - requestedSize

        -- subtract requested from in-flight
        inflightSigs'' =
          Map.merge
            (Map.mapMaybeMissing \_ x -> Just x)
            (Map.mapMaybeMissing \_ _ -> assert False Nothing)
            (Map.zipWithMaybeMatched \_ x y -> assert (x >= y)
                                               let z = x - y in
                                               if z > 0
                                               then Just z
                                               else Nothing)
            (inflightSigs st)
            (Map.fromSet (const 1) requestedSigIds)

        inflightSigsSize'' = assert (inflightSigsSize st >= requestedSize) $
                             inflightSigsSize st - requestedSize

        st' = st { inflightSigs     = inflightSigs'',
                   inflightSigsSize = inflightSigsSize''
                 }

        --
        -- Update PeerSigState
        --

        -- Remove the downloaded `sigid`s from the availableSigIds map, this
        -- guarantees that we won't attempt to download the `sigids` from this peer
        -- once we collect the `sigid`s. Also restrict keys to `liveSet`.
        --
        -- NOTE: we could remove `notReceived` from `availableSigIds`; and
        -- possibly avoid using `unkownSigs` field at all.
        --
        availableSigIds'' = availableSigIds ps
                           `Map.withoutKeys`
                           requestedSigIds

        -- Remove all acknowledged `sigid`s from unknown set, but only those
        -- which are not present in `unacknowledgedSigIds'`
        unkownSigs'' = unkownSigs'
                       `Set.intersection`
                       live
          where
            -- We cannot use `liveSet` as `unknown <> notReceived` might
            -- contain `sigids` which are in `liveSet` but are not `live`.
            live = Set.fromList (toList (unacknowledgedSigIds ps))

        ps'' = ps { availableSigIds           = availableSigIds'',
                    unknownSigs               = unkownSigs'',
                    requestedSigsInflightSize = requestedSigsInflightSize',
                    requestedSigsInflight     = requestedSigsInflight',
                    downloadedSigs            = downloadedSigs' }

--
-- Monadic public API
--

type SharedSigStateVar m peeraddr sigid sig = StrictTVar m (SharedSigState peeraddr sigid sig)

newSharedSigStateVar :: MonadSTM m
                    => StdGen
                    -> m (SharedSigStateVar m peeraddr sigid sig)
newSharedSigStateVar rng = newTVarIO SharedSigState {
    peerSigStates             = Map.empty,
    inflightSigs              = Map.empty,
    inflightSigsSize          = 0,
    bufferedSigs              = Map.empty,
    referenceCounts           = Map.empty,
    timedSigs                 = Map.empty,
    inSubmissionToMempoolSigs = Map.empty,
    peerRng                   = rng
  }


-- | Acknowledge `sigid`s, return the number of `sigids` to be acknowledged to the
-- remote side.
--
receivedSigIds
  :: forall m peeraddr idx sig sigid.
     (MonadSTM m, Ord sigid, Ord peeraddr)
  => Tracer m (TraceSigLogic peeraddr sigid sig)
  -> SharedSigStateVar m peeraddr sigid sig
  -> STM m (MempoolSnapshot sigid sig idx)
  -> peeraddr
  -> NumIdsReq
  -- ^ number of requests to subtract from
  -- `requestedSigIdsInflight`
  -> StrictSeq sigid
  -- ^ sequence of received `sigids`
  -> Map sigid SizeInBytes
  -- ^ received `sigid`s with sizes
  -> m ()
receivedSigIds tracer sharedVar getMempoolSnapshot peeraddr reqNo sigidsSeq sigidsMap = do
  st <- atomically $ do
    MempoolSnapshot{mempoolHasTx} <- getMempoolSnapshot
    stateTVar sharedVar ((\a -> (a,a)) . receivedSigIdsImpl mempoolHasTx peeraddr reqNo sigidsSeq sigidsMap)
  traceWith tracer (TraceSharedTxState "receivedSigIds" st)


-- | Include received `sig`s in `SharedSigState`.  Return number of `sigids`
-- to be acknowledged and list of `sig` to be added to the mempool.
--
collectSigs
  :: forall m peeraddr sig sigid.
     (MonadSTM m, Ord sigid, Ord peeraddr,
      Show sigid, Typeable sigid)
  => Tracer m (TraceSigLogic peeraddr sigid sig)
  -> (sig -> SizeInBytes)
  -> SharedSigStateVar m peeraddr sigid sig
  -> peeraddr
  -> Map sigid SizeInBytes -- ^ set of requested sigids with their announced size
  -> Map sigid sig          -- ^ received sigs
  -> m (Maybe TxSubmissionProtocolError)
  -- ^ number of sigids to be acknowledged and sigs to be added to the
  -- mempool
collectSigs tracer sigSize sharedVar peeraddr sigidsRequested sigsMap = do
  r <- atomically $ do
    st <- readTVar sharedVar
    case collectSigsImpl sigSize peeraddr sigidsRequested sigsMap st of
      r@(Right st') -> writeTVar sharedVar st'
                    $> r
      r@Left {}     -> pure r
  case r of
    Right st -> traceWith tracer (TraceSharedTxState "collectSigs" st)
             $> Nothing
    Left e   -> return (Just e)
