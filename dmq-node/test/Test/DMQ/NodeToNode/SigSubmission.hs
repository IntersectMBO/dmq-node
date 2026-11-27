{-# LANGUAGE RankNTypes #-}

module Test.DMQ.NodeToNode.SigSubmission (tests) where

import Control.Monad (unless, void)
import Control.Monad.Class.MonadAsync (link, withAsync)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import Data.Foldable (toList)
import Data.Singletons (SingI)
import Data.Word (Word16)
import Network.TypedProtocol.Codec (AnyMessage (..))
import Ouroboros.Network.Channel (Channel (..), createConnectedChannels)
import Ouroboros.Network.ControlMessage (ControlMessage (Continue))
import Ouroboros.Network.Driver.Simple (runPeer)
import Ouroboros.Network.Protocol.TxSubmission2.Codec (codecTxSubmission2Id)
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as V1

import DMQ.Mempool qualified as Mempool
import DMQ.NodeToNode.SigSubmission (outboundV1, outboundV2)
import DMQ.NodeToNode.Version (NodeToNodeVersion (..))
import DMQ.Protocol.SigSubmissionV2.Codec (codecSigSubmissionV2Id)
import DMQ.Protocol.SigSubmissionV2.Type qualified as V2

import Test.QuickCheck (Property, once, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "outbound signature bursts"
    [ testProperty (show v) (prop_burst v) | v <- [minBound .. maxBound] ]

data Requests m = Requests {
    requestIds :: forall b. SingI b => V1.SingBlockingStyle b -> Word16 -> m [Int],
    requestBodies :: [Int] -> m [Int]
  }

-- The peer has caught up with signature 10 when the rest of the burst arrives.
-- The tens digit identifies the pool.
prop_burst :: NodeToNodeVersion -> Property
prop_burst version = once $ property $ runSimOrThrow $ do
    mp <- Mempool.new id [10]
    let reader = Mempool.getIndexedReader id (const 1) mp
        writer = Mempool.getWriter () id (\_ -> pure . map Right) (const $ pure ()) mp
    withPeer version reader $ \Requests {requestIds, requestBodies} -> do
      requestIds V1.SingBlocking 0 >>= expect [10]
      requestBodies [10] >>= expect [10]
      void $ Mempool.mempoolAddTxs writer [11, 20, 21]

      if version == NodeToNodeV_3 then do
        start <- getMonotonicTime
        requestIds V1.SingNonBlocking 0 >>= expect [11, 20, 21]
        requestBodies [11, 20, 21] >>= expect [11, 20, 21]
        getMonotonicTime >>= expect start
      else do
        requestIds V1.SingNonBlocking 0 >>= expect [20]
        requestBodies [20] >>= expect [20]
        threadDelay 65
        requestIds V1.SingNonBlocking 0 >>= expect []

        -- A partial acknowledgement releases only the first pool, after 60s.
        requestIds V1.SingNonBlocking 1 >>= expect []
        threadDelay 59
        requestIds V1.SingNonBlocking 0 >>= expect []
        threadDelay 1
        requestIds V1.SingNonBlocking 0 >>= expect [11]
        requestBodies [11] >>= expect [11]

        -- V1 can block until the timer fires. V2 must answer empty blocking
        -- requests on its usual timeout while this pool is cooling down.
        start <- getMonotonicTime
        let awaitIds ack = requestIds V1.SingBlocking ack >>= \case
              []  -> awaitIds 0
              ids -> pure ids
        timeout 61 (awaitIds 2) >>= expect (Just [21])
        end <- getMonotonicTime
        expect 60 (diffTime end start)
        requestBodies [21] >>= expect [21]
      pure True
  where
    expect expected actual = unless (actual == expected) $
      fail ("expected " ++ show expected ++ ", received " ++ show actual)

withPeer :: NodeToNodeVersion
         -> Mempool.TxSubmissionMempoolReader Int Int Integer (IOSim s)
         -> (Requests (IOSim s) -> IOSim s a)
         -> IOSim s a
withPeer NodeToNodeV_1 reader k = do
    (local, remote) <- createConnectedChannels
    let peer = outboundV1 60 (`div` 10) nullTracer 16 reader (pure Continue)
    withAsync (runPeer nullTracer codecTxSubmission2Id local peer) $ \worker -> do
      link worker
      Just (AnyMessage V1.MsgInit) <- recv remote
      k Requests {
          requestIds = \blocking ack -> do
            send remote (AnyMessage (V1.MsgRequestTxIds blocking (fromIntegral ack) 4))
            Just (AnyMessage (V1.MsgReplyTxIds ids)) <- recv remote
            pure (map fst $ toList ids)
        , requestBodies = \ids -> do
            send remote (AnyMessage (V1.MsgRequestTxs ids))
            Just (AnyMessage (V1.MsgReplyTxs bodies)) <- recv remote
            pure bodies
        }
withPeer version reader k = do
    (local, remote) <- createConnectedChannels
    let peer = outboundV2 version 60 (`div` 10) nullTracer 16 reader
    withAsync (runPeer nullTracer codecSigSubmissionV2Id local peer) $ \worker -> do
      link worker
      k Requests {
          requestIds = \blocking ack -> do
            send remote (AnyMessage (V2.MsgRequestSigIds blocking (fromIntegral ack) 4))
            recv remote >>= \case
              Just (AnyMessage (V2.MsgReplySigIds ids)) -> pure (map fst $ toList ids)
              Just (AnyMessage V2.MsgReplyNoSigIds)     -> pure []
              _ -> fail "expected a signature ID reply"
        , requestBodies = \ids -> do
            send remote (AnyMessage (V2.MsgRequestSigs ids))
            Just (AnyMessage (V2.MsgReplySigs bodies)) <- recv remote
            pure bodies
        }
