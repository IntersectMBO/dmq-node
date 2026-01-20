{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.SigSubmission.Inbound.Registry
  ( SigChannels (..)
  , SigChannelsVar
  , SigMempoolSem
  , SharedSigStateVar
  , newSharedSigStateVar
  , newSigChannelsVar
  , newSigMempoolSem
  , PeerSigAPI (..)
  , decisionLogicThreads
  , withPeer
  ) where

import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.TSem
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)

import Data.Foldable as Foldable (foldl', traverse_)
import Data.Hashable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Data.Void (Void)

import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Mempool.Reader
 
import Ouroboros.Network.TxSubmission.Inbound.V2 (TxSubmissionMempoolWriter(..))
 
import DMQ.SigSubmission.Inbound.Decision
import DMQ.SigSubmission.Inbound.Policy
import DMQ.SigSubmission.Inbound.State
import DMQ.SigSubmission.Inbound.Types

-- | Communication channels between `SigSubmission` client mini-protocol and
-- decision logic.
--
newtype SigChannels m peeraddr sigid sig = SigChannels {
      sigChannelMap :: Map peeraddr (StrictMVar m (SigDecision sigid sig))
    }

type SigChannelsVar m peeraddr sigid sig = StrictMVar m (SigChannels m peeraddr sigid sig)

newSigChannelsVar :: MonadMVar m => m (SigChannelsVar m peeraddr sigid sig)
newSigChannelsVar = newMVar (SigChannels Map.empty)

newtype SigMempoolSem m = SigMempoolSem (TSem m)

newSigMempoolSem :: MonadSTM m => m (SigMempoolSem m)
newSigMempoolSem = SigMempoolSem <$> atomically (newTSem 1)

-- | API to access `PeerSigState` inside `PeerSigStateVar`.
--
data PeerSigAPI m sigid sig = PeerSigAPI {
    readSigDecision      :: m (SigDecision sigid sig),
    -- ^ a blocking action which reads `SigDecision`

    handleReceivedSigIds :: NumIdsReq
                         -> StrictSeq sigid
                         -- ^ received sigids
                         -> Map sigid SizeInBytes
                         -- ^ received sizes of advertised sig's
                         -> m (),
    -- ^ handle received sigids

    handleReceivedSigs   :: Map sigid SizeInBytes
                         -- ^ requested sigids
                         -> Map sigid sig
                         -- ^ received sigs
                         -> m (Maybe TxSubmissionProtocolError),
    -- ^ handle received sigs

    submitSigToMempool     :: Tracer m (TraceSigSubmissionInbound sigid sig)
                           -> sigid -> sig -> m ()
    -- ^ submit the given (sigid, sig) to the mempool.
  }


data SigMempoolResult = SigAccepted | SigRejected

-- | A bracket function which registers / de-registers a new peer in
-- `SharedSigStateVar` and `PeerSigStateVar`s,  which exposes `PeerSigStateAPI`.
-- `PeerSigStateAPI` is only safe inside the  `withPeer` scope.
--
withPeer
    :: forall sig peeraddr sigid idx m a failure.
       ( MonadMask m
       , MonadMVar m
       , MonadSTM  m
       , MonadMonotonicTime m
       , Ord sigid
       , Show sigid
       , Typeable sigid
       , Ord peeraddr
       , Show peeraddr
       )
    => Tracer m (TraceSigLogic peeraddr sigid sig)
    -> SigChannelsVar m peeraddr sigid sig
    -> SigMempoolSem m
    -> SigDecisionPolicy
    -> SharedSigStateVar m peeraddr sigid sig
    -> TxSubmissionMempoolReader sigid sig idx m
    -> TxSubmissionMempoolWriter sigid sig idx m failure
    -> (sig -> SizeInBytes)
    -> peeraddr
    --  ^ new peer
    -> (PeerSigAPI m sigid sig -> m a)
    -- ^ callback which gives access to `PeerSigStateAPI`
    -> m a
withPeer tracer
         channelsVar
         (SigMempoolSem mempoolSem)
         policy@SigDecisionPolicy { bufferedSigsMinLifetime }
         sharedStateVar
         TxSubmissionMempoolReader { mempoolGetSnapshot }
         TxSubmissionMempoolWriter { mempoolAddTxs }
         sigSize
         peeraddr io =
    bracket
      (do -- create a communication channel
          !peerSigAPI <-
            modifyMVar channelsVar
              \ SigChannels { sigChannelMap } -> do
                chann <- newEmptyMVar
                let (chann', sigChannelMap') =
                      Map.alterF (\mbChann ->
                                   let !chann'' = fromMaybe chann mbChann
                                   in (chann'', Just chann''))
                                 peeraddr
                                 sigChannelMap
                return
                  ( SigChannels { sigChannelMap = sigChannelMap' }
                  , PeerSigAPI { readSigDecision = takeMVar chann',
                                 handleReceivedSigIds,
                                 handleReceivedSigs,
                                 submitSigToMempool }
                  )

          atomically $ modifyTVar sharedStateVar registerPeer
          return peerSigAPI
      )
      -- the handler is a short blocking operation, thus we need to use
      -- `uninterruptibleMask_`
      (\_ -> uninterruptibleMask_ do
        atomically $ modifyTVar sharedStateVar unregisterPeer
        modifyMVar_ channelsVar
          \ SigChannels { sigChannelMap } ->
            return SigChannels { sigChannelMap = Map.delete peeraddr sigChannelMap }
      )
      io
  where
    registerPeer :: SharedSigState peeraddr sigid sig
                 -> SharedSigState peeraddr sigid sig
    registerPeer st@SharedSigState { peerSigStates } =
      st { peerSigStates =
             Map.insert
               peeraddr
               PeerSigState {
                 availableSigIds           = Map.empty,
                 requestedSigIdsInflight   = 0,
                 requestedSigsInflightSize = 0,
                 requestedSigsInflight     = Set.empty,
                 unacknowledgedSigIds      = StrictSeq.empty,
                 unknownSigs               = Set.empty,
                 score                    = 0,
                 scoreTs                  = Time 0,
                 downloadedSigs            = Map.empty,
                 toMempoolSigs             = Map.empty }
               peerSigStates
         }

    -- TODO: this function needs to be tested!
    -- Issue: https://github.com/IntersectMBO/ouroboros-network/issues/5151
    unregisterPeer :: SharedSigState peeraddr sigid sig
                   -> SharedSigState peeraddr sigid sig
    unregisterPeer st@SharedSigState { peerSigStates,
                                      bufferedSigs,
                                      referenceCounts,
                                      inflightSigs,
                                      inflightSigsSize,
                                      inSubmissionToMempoolSigs } =
        st { peerSigStates             = peerSigStates',
             bufferedSigs              = bufferedSigs',
             referenceCounts          = referenceCounts',
             inflightSigs              = inflightSigs',
             inflightSigsSize          = inflightSigsSize',
             inSubmissionToMempoolSigs = inSubmissionToMempoolSigs' }
      where
        (PeerSigState { unacknowledgedSigIds,
                       requestedSigsInflight,
                       requestedSigsInflightSize,
                       toMempoolSigs }
          , peerSigStates')
          =
          Map.alterF
            (\case
              Nothing -> error ("SigSubmission.withPeer: invariant violation for peer " ++ show peeraddr)
              Just a  -> (a, Nothing))
            peeraddr
            peerSigStates

        referenceCounts' =
          Foldable.foldl'
            (flip $ Map.update \cnt ->
              if cnt > 1
              then Just $! pred cnt
              else Nothing
            )
          referenceCounts
          unacknowledgedSigIds

        liveSet = Map.keysSet referenceCounts'

        bufferedSigs' = bufferedSigs
                       `Map.restrictKeys`
                       liveSet

        inflightSigs' = Foldable.foldl' purgeInflightSigs inflightSigs requestedSigsInflight
        inflightSigsSize' = inflightSigsSize - requestedSigsInflightSize

        -- When we unregister a peer, we need to subtract all sigs in the
        -- `toMempoolSigs`, as they will not be submitted to the mempool.
        inSubmissionToMempoolSigs' =
          Foldable.foldl' (flip $ Map.update \cnt ->
               if cnt > 1
               then Just $! pred cnt
               else Nothing
            )
          inSubmissionToMempoolSigs
          (Map.keysSet toMempoolSigs)

        purgeInflightSigs m sigid = Map.alter fn sigid m
          where
            fn (Just n) | n > 1 = Just $! pred n
            fn _                = Nothing

    --
    -- PeerSigAPI
    --

    submitSigToMempool :: Tracer m (TraceSigSubmissionInbound sigid sig) -> sigid -> sig -> m ()
    submitSigToMempool sigTracer sigid sig =
        bracket_ (atomically $ waitTSem mempoolSem)
                 (atomically $ signalTSem mempoolSem)
          $ do
            start <- getMonotonicTime
            res <- addSig
            end <- getMonotonicTime
            atomically $ modifyTVar sharedStateVar (updateBufferedSig end res)
            let duration = end `diffTime` start
            case res of
              SigAccepted -> traceWith sigTracer (TraceSigInboundAddedToMempool [sigid] duration)
              SigRejected -> traceWith sigTracer (TraceSigInboundRejectedFromMempool [sigid] duration)

      where
        -- add the sig to the mempool
        addSig :: m SigMempoolResult
        addSig = do
          mpSnapshot <- atomically mempoolGetSnapshot

          -- Note that checking if the mempool contains a sig before
          -- spending several ms attempting to add it to the pool has
          -- been judged immoral.
          if mempoolHasTx mpSnapshot sigid
             then do
               !now <- getMonotonicTime
               !s <- countRejectedSigs now 1
               traceWith sigTracer $ TraceSigSubmissionProcessed ProcessedTxCount {
                    ptxcAccepted = 0
                  , ptxcRejected = 1
                  , ptxcScore    = s
                  }
               return SigRejected
             else do
               acceptedSigs <- mempoolAddTxs [sig]
               end <- getMonotonicTime
               if null acceptedSigs
                  then do
                      !s <- countRejectedSigs end 1
                      traceWith sigTracer $ TraceSigSubmissionProcessed ProcessedTxCount {
                          ptxcAccepted = 0
                        , ptxcRejected = 1
                        , ptxcScore    = s
                        }
                      return SigRejected
                  else do
                      !s <- countRejectedSigs end 0
                      traceWith sigTracer $ TraceSigSubmissionProcessed ProcessedTxCount {
                          ptxcAccepted = 1
                        , ptxcRejected = 0
                        , ptxcScore    = s
                        }
                      return SigAccepted

        updateBufferedSig :: Time
                          -> SigMempoolResult
                          -> SharedSigState peeraddr sigid sig
                          -> SharedSigState peeraddr sigid sig
        updateBufferedSig _ SigRejected st@SharedSigState { peerSigStates
                                                         , inSubmissionToMempoolSigs } =
            st { peerSigStates = peerSigStates'
               , inSubmissionToMempoolSigs = inSubmissionToMempoolSigs' }
          where
            inSubmissionToMempoolSigs' =
              Map.update (\case 1 -> Nothing; n -> Just $! pred n)
                         sigid inSubmissionToMempoolSigs

            peerSigStates' = Map.update fn peeraddr peerSigStates
              where
                fn ps = Just $! ps { toMempoolSigs = Map.delete sigid (toMempoolSigs ps)}

        updateBufferedSig now SigAccepted
                         st@SharedSigState { peerSigStates
                                          , bufferedSigs
                                          , referenceCounts
                                          , timedSigs
                                          , inSubmissionToMempoolSigs } =
            st { peerSigStates = peerSigStates'
               , bufferedSigs = bufferedSigs'
               , timedSigs = timedSigs'
               , referenceCounts = referenceCounts'
               , inSubmissionToMempoolSigs = inSubmissionToMempoolSigs'
               }
          where
            inSubmissionToMempoolSigs' =
              Map.update (\case 1 -> Nothing; n -> Just $! pred n)
                         sigid inSubmissionToMempoolSigs

            timedSigs' = Map.alter fn (addTime bufferedSigsMinLifetime now) timedSigs
              where
                fn :: Maybe [sigid] -> Maybe [sigid]
                fn Nothing      = Just [sigid]
                fn (Just sigids) = Just $! (sigid:sigids)

            referenceCounts' = Map.alter fn sigid referenceCounts
              where
                fn :: Maybe Int -> Maybe Int
                fn Nothing  = Just 1
                fn (Just n) = Just $! succ n

            bufferedSigs' =  Map.insert sigid (Just sig) bufferedSigs

            peerSigStates' = Map.update fn peeraddr peerSigStates
              where
                fn ps = Just $! ps { toMempoolSigs = Map.delete sigid (toMempoolSigs ps)}

    handleReceivedSigIds :: NumIdsReq
                         -> StrictSeq sigid
                         -> Map sigid SizeInBytes
                         -> m ()
    handleReceivedSigIds numIdsReq sigidsSeq sigidsMap =
      receivedSigIds tracer
                     sharedStateVar
                     mempoolGetSnapshot
                     peeraddr
                     numIdsReq
                     sigidsSeq
                     sigidsMap


    handleReceivedSigs :: Map sigid SizeInBytes
                       -- ^ requested sigids with their announced size
                       -> Map sigid sig
                       -- ^ received sigs
                       -> m (Maybe TxSubmissionProtocolError)
    handleReceivedSigs =
      collectSigs tracer sigSize sharedStateVar peeraddr
      
    -- Update `score` & `scoreTs` fields of `PeerSigState`, return the new
    -- updated `score`.
    --
    -- PRECONDITION: the `Double` argument is non-negative.
    countRejectedSigs :: Time
                     -> Double
                     -> m Double
    countRejectedSigs _ n | n < 0 =
      error ("SigSubmission.countRejectedSigs: invariant violation for peer " ++ show peeraddr)
    countRejectedSigs now n = atomically $ stateTVar sharedStateVar $ \st ->
        let (result, peerSigStates') = Map.alterF fn peeraddr (peerSigStates st)
        in (result, st { peerSigStates = peerSigStates' })
      where
        fn :: Maybe (PeerSigState sigid sig) -> (Double, Maybe (PeerSigState sigid sig))
        fn Nothing   = error ("SigSubmission.withPeer: invariant violation for peer " ++ show peeraddr)
        fn (Just ps) = (score ps', Just $! ps')
          where
            ps' = updateRejects policy now n ps


updateRejects :: SigDecisionPolicy
              -> Time
              -> Double
              -> PeerSigState sigid sig
              -> PeerSigState sigid sig
updateRejects _ now 0 pts | score pts == 0 = pts {scoreTs = now}
updateRejects SigDecisionPolicy { scoreRate, scoreMax } now n
              pts@PeerSigState { score, scoreTs } =
    let duration = diffTime now scoreTs
        !drain   = realToFrac duration * scoreRate
        !drained = max 0 $ score - drain in
    pts { score   = min scoreMax $ drained + n
        , scoreTs = now
        }


drainRejectionThread
    :: forall m peeraddr sigid sig.
       ( MonadDelay m
       , MonadSTM m
       , MonadThread m
       , Ord sigid
       )
    => Tracer m (TraceSigLogic peeraddr sigid sig)
    -> SigDecisionPolicy
    -> SharedSigStateVar m peeraddr sigid sig
    -> m Void
drainRejectionThread tracer policy sharedStateVar = do
    labelThisThread "sig-rejection-drain"
    now <- getMonotonicTime
    go $ addTime drainInterval now
  where
    drainInterval :: DiffTime
    drainInterval = 7

    go :: Time -> m Void
    go !nextDrain = do
      threadDelay 1

      !now <- getMonotonicTime
      st'' <- atomically $ do
        st <- readTVar sharedStateVar
        let ptss = if now > nextDrain then Map.map (updateRejects policy now 0) (peerSigStates st)
                                      else peerSigStates st
            st' = tickTimedSigs now st
                    { peerSigStates = ptss }
        writeTVar sharedStateVar st'
        return st'
      traceWith tracer (TraceSharedTxState "drainRejectionThread" st'')

      if now > nextDrain
         then go $ addTime drainInterval now
         else go nextDrain


decisionLogicThread
    :: forall m peeraddr sigid sig.
       ( MonadDelay m
       , MonadMVar  m
       , MonadSTM   m
       , MonadMask  m
       , MonadFork  m
       , Ord peeraddr
       , Ord sigid
       , Hashable peeraddr
       )
    => Tracer m (TraceSigLogic peeraddr sigid sig)
    -> Tracer m TxSubmissionCounters
    -> SigDecisionPolicy
    -> SigChannelsVar m peeraddr sigid sig
    -> SharedSigStateVar m peeraddr sigid sig
    -> m Void
decisionLogicThread tracer counterTracer policy sigChannelsVar sharedStateVar = do
    labelThisThread "sig-decision"
    go
  where
    go :: m Void
    go = do
      -- We rate limit the decision making process, it could overwhelm the CPU
      -- if there are too many inbound connections.
      threadDelay _DECISION_LOOP_DELAY

      (decisions, st) <- atomically do
        sharedSigState <- readTVar sharedStateVar
        let activePeers = filterActivePeers policy sharedSigState

        -- block until at least one peer is active
        check (not (Map.null activePeers))

        let (sharedState, decisions) = makeDecisions policy sharedSigState activePeers
        writeTVar sharedStateVar sharedState
        return (decisions, sharedState)
      traceWith tracer (TraceSharedTxState "decisionLogicThread" st)
      traceWith tracer (TraceSigDecisions decisions)
      SigChannels { sigChannelMap } <- readMVar sigChannelsVar
      traverse_
        (\(mvar, d) -> modifyMVarWithDefault_ mvar d (\d' -> pure (d' <> d)))
        (Map.intersectionWith (,)
          sigChannelMap
          decisions)
      traceWith counterTracer (mkTxSubmissionCounters st)
      go

    -- Variant of modifyMVar_ that puts a default value if the MVar is empty.
    modifyMVarWithDefault_ :: StrictMVar m a -> a -> (a -> m a) -> m ()
    modifyMVarWithDefault_ m d io =
      mask $ \restore -> do
        mbA  <- tryTakeMVar m
        case mbA of
          Just a -> do
            a' <- restore (io a) `onException` putMVar m a
            putMVar m a'
          Nothing -> putMVar m d


-- | Run `decisionLogicThread` and `drainRejectionThread`.
--
decisionLogicThreads
    :: forall m peeraddr sigid sig.
       ( MonadDelay m
       , MonadMVar  m
       , MonadMask  m
       , MonadAsync m
       , MonadFork  m
       , Ord peeraddr
       , Ord sigid
       , Hashable peeraddr
       )
    => Tracer m (TraceSigLogic peeraddr sigid sig)
    -> Tracer m TxSubmissionCounters
    -> SigDecisionPolicy
    -> SigChannelsVar m peeraddr sigid sig
    -> SharedSigStateVar m peeraddr sigid sig
    -> m Void
decisionLogicThreads tracer counterTracer policy sigChannelsVar sharedStateVar =
  uncurry (<>) <$>
    drainRejectionThread tracer policy sharedStateVar
    `concurrently`
    decisionLogicThread tracer counterTracer policy sigChannelsVar sharedStateVar


-- `5ms` delay
_DECISION_LOOP_DELAY :: DiffTime
_DECISION_LOOP_DELAY = 0.005
