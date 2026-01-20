{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.SigSubmission.App (tests) where

import Prelude hiding (seq)

import System.Random (mkStdGen)
import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Tracer (Tracer (..), contramap)

import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Hashable
import Data.List (nubBy)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Typeable (Typeable)

import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Driver
import Ouroboros.Network.Util.ShowProxy

import DMQ.SigSubmission.Outbound (sigSubmissionOutbound)
import DMQ.SigSubmission.Inbound (SigChannels(..), sigSubmissionInboundV2,
           PeerSigAPI)
import DMQ.SigSubmission.Inbound.Types (SigSubmissionInitDelay(..),
           TraceSigLogic)
import DMQ.SigSubmission.Inbound.Policy (SigDecisionPolicy,
           maxUnacknowledgedSigIds)
import DMQ.Protocol.SigSubmissionV2.Type (NumIdsAck(..), SigSubmissionV2,
           getNumIdsReq)
import DMQ.Protocol.SigSubmissionV2.Codec (byteLimitsSigSubmissionV2,
           timeLimitsSigSubmissionV2)
import DMQ.Protocol.SigSubmissionV2.Outbound (sigSubmissionV2OutboundPeer)
import DMQ.Protocol.SigSubmissionV2.Inbound (
           sigSubmissionV2InboundPeerPipelined)
import DMQ.SigSubmission.Inbound.State (SharedSigState, newSharedSigStateVar)
import DMQ.SigSubmission.Inbound.Registry (decisionLogicThreads, withPeer,
           newSigMempoolSem)

import Test.Ouroboros.Network.Utils hiding (debugTracer)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.DMQ.SigSubmission.SigLogic (SigSubmissionState (..))
import Test.DMQ.SigSubmission.Types (SigId, Sig (..), debugTracer,
           verboseTracer, newMempool, emptyMempool, readMempool,
           sigSubmissionCodec2, getMempoolReader, getMempoolWriter)

tests :: TestTree
tests = testGroup "Test.DMQ.SigSubmission.App"
  [ testProperty "sigSubmission"  prop_sigSubmission
  ]


data TestVersion = TestVersion
  deriving (Eq, Ord, Bounded, Enum, Show)


-- | Tests overall sig submission semantics.
-- This property test is the same as for tx submission v1. We need this to know
-- we didn't regress.
--
prop_sigSubmission :: SigSubmissionState -> Property
prop_sigSubmission st@(SigSubmissionState peers _) =
    let tr = runSimTrace (sigSubmissionSimulation st)
        numPeersWithWronglySizedSig :: Int
        numPeersWithWronglySizedSig =
          foldr
            (\(sigs, _, _) r ->
              case List.find (\sig -> getSigSize sig /= getSigAdvSize sig) sigs of
                Just {} -> r + 1
                Nothing -> r
            ) 0 peers
    in
        label ("number of peers: " ++ renderRanges 3 (Map.size peers))
      . label ("number of sigs: "
              ++
              renderRanges 10
                ( Set.size
                . foldMap (Set.fromList . (\(sigs, _, _) -> getSigId <$> sigs))
                $ Map.elems peers
                ))
      . label ("number of peers with wrongly sized sig: "
             ++ show numPeersWithWronglySizedSig)
      $ case traceResult True tr of
         Left e ->
             counterexample (show e)
           . counterexample (ppTrace tr)
           $ False
         Right (inmp, outmps) ->
             counterexample (ppTrace tr)
           $ conjoin (validate inmp `map` outmps)
  where
    validate :: [Sig SigId] -- the inbound mempool
             -> [Sig SigId] -- one of the outbound mempools
             -> Property
    validate inmp outmp =
       let outUniqueSigIds = nubBy (on (==) getSigId) outmp
           outValidSigs    = filterValidSigs outmp
       in
       case ( length outUniqueSigIds == length outmp
            , length outValidSigs == length outmp
            ) of
         x@(True, True) ->
           -- If we are presented with a stream of unique sigids for valid
           -- signatures the inbound signatures should match the outbound
           -- signatures exactly.
             counterexample (show x)
           . counterexample (show inmp)
           . counterexample (show outmp)
           $ checkMempools inmp (take (length inmp) outValidSigs)

         x@(True, False) | Nothing <- List.find (\sig -> getSigAdvSize sig /= getSigSize sig) outmp  ->
           -- If we are presented with a stream of unique sigids then we should have
           -- fetched all valid signatures if all sigs have valid sizes.
             counterexample (show x)
           . counterexample (show inmp)
           . counterexample (show outValidSigs)

           $ checkMempools inmp (take (length inmp) outValidSigs)
                         | otherwise ->
             -- If there's one sig with an invalid size, we will download only
             -- some of them, but we don't guarantee how many we will download.
             --
             -- This is ok, the peer is cheating.
             property True


         x@(False, True) ->
           -- If we are presented with a stream of valid sigids then we should have
           -- fetched some version of those signatures.
             counterexample (show x)
           . counterexample (show inmp)
           . counterexample (show outmp)
           $ checkMempools (map getSigId inmp)
                           (take (length inmp)
                                 (getSigId <$> filterValidSigs outUniqueSigIds))

         (False, False) ->
           -- If we are presented with a stream of valid and invalid Sigs with
           -- duplicate sigids we're content with completing the protocol
           -- without error.
           property True


sigSubmissionSimulation :: forall s . SigSubmissionState
                       -> IOSim s ([Sig SigId], [[Sig SigId]])
                       -- ^ inbound & outbound mempools
sigSubmissionSimulation (SigSubmissionState state sigDecisionPolicy) = do
  state' <- traverse (\(sigs, mbOutDelay, mbInDelay) -> do
                      let mbOutDelayTime = getSmallDelay . getPositive <$> mbOutDelay
                          mbInDelayTime  = getSmallDelay . getPositive <$> mbInDelay
                      controlMessageVar <- newTVarIO Continue
                      return ( sigs
                             , controlMessageVar
                             , mbOutDelayTime
                             , mbInDelayTime
                             )
                    )
                    state

  state'' <- traverse (\(sigs, var, mbOutDelay, mbInDelay) -> do
                       return ( sigs
                              , readTVar var
                              , mbOutDelay
                              , mbInDelay
                              )
                    )
                    state'

  let simDelayTime = Map.foldl' (\m (sigs, _, mbInDelay, mbOutDelay) ->
                                  max m ( fromMaybe 1 (max <$> mbInDelay <*> mbOutDelay)
                                        * realToFrac (length sigs `div` 4)
                                        )
                                )
                                0
                                state''
      controlMessageVars = (\(_, x, _, _) -> x)
                        <$> Map.elems state'

  withAsync
    (do threadDelay (simDelayTime + 1000)
        atomically (traverse_ (`writeTVar` Terminate) controlMessageVars)
    ) \_ -> do
      let tracer :: forall a. (Show a, Typeable a) => Tracer (IOSim s) a
          tracer = verboseTracer
                <> debugTracer
                <> Tracer traceM
      runSigSubmission tracer tracer state'' sigDecisionPolicy


filterValidSigs :: [Sig SigId] -> [Sig SigId]
filterValidSigs
  = filter getSigValid
  . takeWhile (\Sig{getSigSize, getSigAdvSize} -> getSigSize == getSigAdvSize)
  

-- | Check that the inbound mempool contains all outbound `sig`s as a proper
-- subsequence.  It might contain more `sig`s from other peers.
--
checkMempools :: Eq sig
              => [sig] -- inbound mempool
              -> [sig] -- outbound mempool
              -> Bool
checkMempools _  []    = True  -- all outbound `sig` were found in the inbound
                               -- mempool
checkMempools [] (_:_) = False -- outbound mempool contains `sig`s which were
                               -- not transferred to the inbound mempool
checkMempools (i : is') os@(o : os')
  | i == o
  = checkMempools is' os'

  | otherwise
  -- `_i` is not present in the outbound mempool, we can skip it.
  = checkMempools is' os


newtype SigStateTrace peeraddr sigid =
    SigStateTrace (SharedSigState peeraddr sigid (Sig sigid))

runSigSubmission
  :: forall m peeraddr sigid.
     ( MonadAsync m
     , MonadDelay m
     , MonadFork  m
     , MonadMask  m
     , MonadMVar  m
     , MonadSay   m
     , MonadST    m
     , MonadLabelledSTM m
     , MonadTime m
     , MonadTimer m
     , MonadThrow (STM m)
     , MonadTraceSTM m
     , ShowProxy sigid
     , Typeable sigid
     , Show peeraddr
     , Ord peeraddr
     , Hashable peeraddr
     , Typeable peeraddr

     , sigid ~ Int
     )
  => Tracer m (String, TraceSendRecv (SigSubmissionV2 sigid (Sig sigid)))
  -> Tracer m (TraceSigLogic peeraddr sigid (Sig sigid))
  -> Map peeraddr ( [Sig sigid]
                  , ControlMessageSTM m
                  , Maybe DiffTime
                  , Maybe DiffTime
                  )
  -> SigDecisionPolicy
  -> m ([Sig sigid], [[Sig sigid]])
  -- ^ inbound and outbound mempools
runSigSubmission tracer tracerSigLogic st0 sigDecisionPolicy = do
    st <- traverse (\(b, c, d, e) -> do
        mempool <- newMempool b
        (outChannel, inChannel) <- createConnectedChannels
        return (mempool, c, d, e, outChannel, inChannel)
        ) st0
    inboundMempool <- emptyMempool
    let sigRng = mkStdGen 42 -- TODO

    sigChannelsVar <- newMVar (SigChannels Map.empty)
    sigMempoolSem <- newSigMempoolSem
    sharedSigStateVar <- newSharedSigStateVar sigRng
    traceTVarIO sharedSigStateVar \_ -> return . TraceDynamic . SigStateTrace
    labelTVarIO sharedSigStateVar "shared-sig-state"

    withAsync (decisionLogicThreads tracerSigLogic sayTracer
                                    sigDecisionPolicy sigChannelsVar sharedSigStateVar) $ \a -> do
      let servers = (\(addr, (mempool, _, _, inDelay, _, inChannel)) -> do
                      let server = sigSubmissionOutbound
                                     (Tracer $ say . show)
                                     (NumIdsAck $ getNumIdsReq $ maxUnacknowledgedSigIds sigDecisionPolicy)
                                     (getMempoolReader mempool)
                                     (maxBound :: TestVersion)
                      runPeerWithLimits
                        (("OUTBOUND " ++ show addr,) `contramap` tracer)
                        sigSubmissionCodec2
                        (byteLimitsSigSubmissionV2 (fromIntegral . BSL.length))
                        timeLimitsSigSubmissionV2
                        (maybe id delayChannel inDelay inChannel)
                        (sigSubmissionV2OutboundPeer server)
                    )
                   <$> Map.assocs st

      let clients = (\(addr, (_, ctrlMsgSTM, outDelay, _, outChannel, _)) -> do
                       withPeer tracerSigLogic
                                sigChannelsVar
                                sigMempoolSem
                                sigDecisionPolicy
                                sharedSigStateVar
                                (getMempoolReader inboundMempool)
                                (getMempoolWriter inboundMempool)
                                getSigSize
                                addr $ \(api :: PeerSigAPI m SigId (Sig SigId))-> do
                                  let client = sigSubmissionInboundV2
                                                 verboseTracer
                                                 NoSigSubmissionInitDelay
                                                 (getMempoolWriter inboundMempool)
                                                 api
                                                 ctrlMsgSTM
                                  runPipelinedPeerWithLimits
                                    (("INBOUND " ++ show addr,) `contramap` verboseTracer)
                                    sigSubmissionCodec2
                                    (byteLimitsSigSubmissionV2 (fromIntegral . BSL.length))
                                    timeLimitsSigSubmissionV2
                                    (maybe id delayChannel outDelay outChannel)
                                    (sigSubmissionV2InboundPeerPipelined client)
                    ) <$> Map.assocs st

      -- Run clients and servers
      withAsyncAll (zip clients servers) $ \as -> do
        _ <- waitAllClients as
        -- cancel decision logic thread
        cancel a

        inmp <- readMempool inboundMempool
        let outmp = map (\(sigs, _, _, _) -> sigs)
                  $ Map.elems st0

        return (inmp, outmp)
  where
    waitAllClients :: [(Async m x, Async m x)] -> m [Either SomeException x]
    waitAllClients [] = return []
    waitAllClients ((client, server):as) = do
      r <- waitCatch client
      -- cancel server as soon as the client exits
      cancel server
      rs <- waitAllClients as
      return (r : rs)

    withAsyncAll :: [(m a, m a)]
                 -> ([(Async m a, Async m a)] -> m b)
                 -> m b
    withAsyncAll xs0 action = go [] xs0
      where
        go as []         = action (reverse as)
        go as ((x,y):xs) = withAsync x (\a -> withAsync y (\b -> go ((a, b):as) xs))
