{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE PackageImports #-}

module DMQ.NodeToNode.SigSubmission
  ( outboundV1
  , outboundV2
  ) where

import Control.Concurrent.Class.MonadSTM qualified as STM
import Control.Monad (when)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Class.MonadTimer.SI
import "contra-tracer" Control.Tracer (Tracer)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Network.TypedProtocol.Core (IsPipelined (NonPipelined),
           PeerRole (AsClient, AsServer))
import Network.TypedProtocol.Peer (Peer)

import DMQ.NodeToNode.Version (NodeToNodeVersion (..))
import DMQ.Protocol.SigSubmissionV2.Outbound
import DMQ.Protocol.SigSubmissionV2.Type qualified as V2
import DMQ.SigSubmissionV2.Outbound (TraceSigSubmissionOutbound,
           sigSubmissionOutbound)

import Ouroboros.Network.ControlMessage (ControlMessageSTM)
import Ouroboros.Network.Protocol.TxSubmission2.Client qualified as V1
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as V1
import Ouroboros.Network.SizeInBytes (SizeInBytes)
import Ouroboros.Network.TxSubmission.Mempool.Reader
import Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound,
           txSubmissionOutbound)

outboundV1
  :: forall m idx sigId sig pool.
     (MonadTimer m, MonadThrow m, Ord idx, Ord sigId, Show sigId, Ord pool)
  => DiffTime -> (sig -> pool)
  -> Tracer m (TraceTxSubmissionOutbound sigId sig) -> V1.NumTxIdsToAck
  -> TxSubmissionMempoolReader sigId sig idx m -> ControlMessageSTM m
  -> Peer (V1.TxSubmission2 sigId sig) AsClient NonPipelined V1.StInit m ()
outboundV1 delay poolOf tracer maxUnacked reader controlMessage =
    V1.txSubmissionClientPeer $
      if delay <= 0
      then client reader
      else V1.TxSubmissionClient $ do
        (paced, acknowledge) <- newPacedReader delay poolOf reader
        V1.runTxSubmissionClient (paceLegacyClient acknowledge (client paced))
  where
    client :: Ord i => TxSubmissionMempoolReader sigId sig i m
           -> V1.TxSubmissionClient sigId sig m ()
    client r = txSubmissionOutbound tracer maxUnacked r NodeToNodeV_1 controlMessage

-- | V2 and V3 share the SigSubmissionV2 wire protocol. Only V3 peers can
-- receive bursts. Both inbound protocols use the same validating writer.
outboundV2
  :: forall m idx sigId sig pool.
     (MonadTimer m, MonadThrow m, Ord idx, Ord sigId, Show sigId, Ord pool)
  => NodeToNodeVersion -> DiffTime -> (sig -> pool)
  -> Tracer m (TraceSigSubmissionOutbound sigId sig) -> V2.NumIdsAck
  -> TxSubmissionMempoolReader sigId sig idx m
  -> Peer (V2.SigSubmissionV2 sigId sig) AsServer NonPipelined V2.StIdle m ()
outboundV2 version delay poolOf tracer maxUnacked reader =
    sigSubmissionV2OutboundPeer $
      case version of
        NodeToNodeV_1 -> pacedOutbound
        NodeToNodeV_2 -> pacedOutbound
        NodeToNodeV_3 -> outbound reader
  where
    pacedOutbound :: SigSubmissionOutbound sigId sig m ()
    pacedOutbound
      | delay <= 0 = outbound reader
      | otherwise = SigSubmissionOutbound $ do
          (pacedReader, acknowledge) <- newPacedReader delay poolOf reader
          let acknowledgeSigIds = acknowledge . V1.NumTxIdsToAck . V2.getNumIdsAck
          runSigSubmissionOutbound (paceLegacyOutbound acknowledgeSigIds (outbound pacedReader))

    outbound :: Ord i
             => TxSubmissionMempoolReader sigId sig i m
             -> SigSubmissionOutbound sigId sig m ()
    outbound r = sigSubmissionOutbound tracer maxUnacked r version

-- | Notify the paced reader before answering each ID request.
paceLegacyOutbound :: Monad m
                   => (V2.NumIdsAck -> m ())
                   -> SigSubmissionOutbound sigId sig m a
                   -> SigSubmissionOutbound sigId sig m a
paceLegacyOutbound acknowledge (SigSubmissionOutbound start) =
    SigSubmissionOutbound (pace <$> start)
  where
    pace idle = idle {
        recvMsgRequestSigIds = \blocking ack req -> do
          acknowledge ack
          recvMsgRequestSigIds idle blocking ack req >>= \case
            SendMsgReplySigIds ids next -> pure (SendMsgReplySigIds ids (pace next))
            SendMsgReplyNoSigIds next   -> pure (SendMsgReplyNoSigIds (pace next))
      , recvMsgRequestSigs = \ids -> do
          SendMsgReplySigs sigs next <- recvMsgRequestSigs idle ids
          pure (SendMsgReplySigs sigs (pace next))
      }

-- | V1 carries the acknowledgement on the TxSubmission client.
paceLegacyClient :: Monad m
                 => (V1.NumTxIdsToAck -> m ())
                 -> V1.TxSubmissionClient sigId sig m a
                 -> V1.TxSubmissionClient sigId sig m a
paceLegacyClient acknowledge (V1.TxSubmissionClient start) =
    V1.TxSubmissionClient (pace <$> start)
  where
    pace idle = V1.ClientStIdle {
        V1.recvMsgRequestTxIds = \blocking ack req -> do
          acknowledge ack
          V1.recvMsgRequestTxIds idle blocking ack req >>= \case
            V1.SendMsgReplyTxIds ids next -> pure (V1.SendMsgReplyTxIds ids (pace next))
            done@V1.SendMsgDone{}         -> pure done
      , V1.recvMsgRequestTxs = \ids -> do
          V1.SendMsgReplyTxs sigs next <- V1.recvMsgRequestTxs idle ids
          pure (V1.SendMsgReplyTxs sigs (pace next))
      }

-- | Position in a connection's announcement order. Zero precedes the first
-- announcement.
newtype AnnouncementId = AnnouncementId Integer
  deriving (Eq, Ord)

advanceAnnouncementId
  :: AnnouncementId
  -> V1.NumTxIdsToAck
  -> AnnouncementId
advanceAnnouncementId (AnnouncementId current) count =
    AnnouncementId (current + toInteger count)

-- | The submission protocols advance a cursor only for IDs actually announced.
-- The first field orders announcements even when a deferred signature has a
-- smaller mempool index than one already sent for another pool.
data Cursor idx sigId pool = Cursor
    !AnnouncementId                -- announcement number
    !idx                           -- index for looking up the body
    !idx                           -- highest mempool index visited
    !(Map idx (sigId, SizeInBytes)) -- deferred entries, in insertion order
    !(Map pool AnnouncementId)     -- last announcement still holding each pool back
  deriving (Eq, Ord)

-- | A peer may collect or request bodies long after it learns their IDs. Start
-- the delay at its FIFO acknowledgement, so the peer has finished processing
-- the previous ID before the next one for that pool becomes available.
newPacedReader
  :: forall m sigId sig idx pool.
     (MonadTimer m, Ord idx, Ord pool)
  => DiffTime
  -> (sig -> pool)
  -> TxSubmissionMempoolReader sigId sig idx m
  -> m ( TxSubmissionMempoolReader sigId sig (Cursor idx sigId pool) m
       , V1.NumTxIdsToAck -> m ()
       )
newPacedReader delay poolOf reader = do
    -- A whole acknowledgement batch shares one timer. A blocking ID request
    -- wakes when the oldest timer fires independently of mempool changes.
    acks <- STM.newTVarIO (AnnouncementId 0, Seq.empty)
    let acknowledge :: V1.NumTxIdsToAck -> m ()
        acknowledge n = when (n > 0) $ do
          !timer <- registerDelay delay
          STM.atomically $ STM.modifyTVar' acks $ \(ready, waiting) ->
            let !waiting' = waiting Seq.|> (n, timer) in
            (ready, waiting')

        -- The running total spans every acknowledgement batch on the connection.
        advance :: AnnouncementId
                -- ^ Last announcement whose acknowledgement delay has elapsed.
                -- Return this position at the first pending timer.
                -> Seq.Seq (V1.NumTxIdsToAck, STM.TVar m Bool)
                -> STM.STM m (AnnouncementId, Seq.Seq (V1.NumTxIdsToAck, STM.TVar m Bool))
        advance !ready waiting = case Seq.viewl waiting of
          Seq.EmptyL -> pure (ready, waiting)
          (n, timer) Seq.:< rest -> do
            elapsed <- STM.readTVar timer
            if elapsed
              then -- client acknowledged `n` sigids, we can advance the
                   -- `AnnoucmentId`
                   advance (advanceAnnouncementId ready n) rest
              else pure (ready, waiting)

        paced = TxSubmissionMempoolReader {
            mempoolGetSnapshot = do
              (ready, waiting) <- STM.readTVar acks
              (ready', waiting') <- advance ready waiting
              when (ready /= ready') $ STM.writeTVar acks (ready', waiting')
              pacedSnapshot poolOf ready' <$> mempoolGetSnapshot reader
          , mempoolZeroIdx = Cursor (AnnouncementId 0) zero zero Map.empty Map.empty
          }
    pure (paced, acknowledge)
  where
    zero = mempoolZeroIdx reader

-- | Reconsider deferred entries before fresh ones, so each pool retains its
-- mempool order while other pools can pass it. The submission protocol keeps
-- the cursor of the last ID that fits in the reply.
pacedSnapshot
  :: forall pool sigId sig idx.
     (Ord idx, Ord pool)
  => (sig -> pool)
  -- ^ @poolOf@ identifies the pool that issued a signature.
  -> AnnouncementId
  -- ^ An ID counts towards @ready@ once a full pacing delay has elapsed
  -- since the peer acknowledged it on this connection.  All `AnnouncementId`s
  -- smaller or equal to it can be released.
  -> MempoolSnapshot sigId sig idx
  -- ^ @mempoolSnapshot@ lists IDs in mempool order and looks up signature
  -- bodies by their original mempool index.
  -> MempoolSnapshot sigId sig (Cursor idx sigId pool)
  -- ^ Each eligible ID has a cursor for looking up its body and resuming
  -- the scan.
pacedSnapshot poolOf ready mempoolSnapshot =
  mempoolSnapshot
    { mempoolTxIdsAfter = \(Cursor annId _ high deferred held) ->
       let fresh = [ (idx, (sigid, size))
                   | (sigid, idx, size) <- mempoolTxIdsAfter mempoolSnapshot high ]
       in offers annId high deferred
                 -- release all held entries with announcement ID smaller or
                 -- equal to `ready`.  They were already acknowledged by the
                 -- peer and the pacing delay has elapsed.
                 (Map.filter (> ready) held)
                 (Map.toAscList deferred ++ fresh)
    , mempoolLookupTx = \(Cursor _ idx _ _ _) -> mempoolLookupTx mempoolSnapshot idx
    }
  where
    offers :: AnnouncementId
           -- ^ @annId@ is the announcement ID of the current `Cursor`.  Each
           -- new offer advances it by one.
           -> idx
           -- ^ @high@ is the greatest mempool index examined so far, including
           -- deferred entries. A fresh scan reads new entries after this index.
           -> Map idx (sigId, SizeInBytes)
           -- ^ @deferred@ keeps IDs and sizes skipped while their pools were
           -- held, keyed by mempool index so they can be retried in order.
           -> Map pool AnnouncementId
           -- ^ @held@ records the last announcement number for each held pool.
           -- Its next ID is eligible once @ready@ reaches that number.
           -> [(idx, (sigId, SizeInBytes))]
           -- ^ Candidates still to examine. Deferred entries in ascending index
           -- order come before fresh entries from the snapshot.
           -> [(sigId, Cursor idx sigId pool, SizeInBytes)]
    offers _ _ _ _ [] = []
    offers annId !high !deferred held ((idx, entry@(!sigid, !size)) : rest) =
      case poolOf <$> mempoolLookupTx mempoolSnapshot idx of
        Nothing -> continue annId (Map.delete idx deferred) held
        Just pool
          | pool `Map.member` held ->
              continue annId (Map.insert idx entry deferred) held
          | otherwise ->
              -- the `sigId` can be released; advance the announcement ID,
              -- updated `deferred` and `held` maps
              let annId' = advanceAnnouncementId annId 1
                  deferred' = Map.delete idx deferred
                  -- update `held` with advanced `annId'`, it will be held
                  -- until the `ready :: AnnouncementId` advances.
                  held' = Map.insert pool annId' held
              in (sigid, Cursor annId' idx high' deferred' held', size)
               : continue annId' deferred' held'
      where
        high' = max high idx
        continue nextAnnId nextDeferred nextHeld =
          offers nextAnnId high' nextDeferred nextHeld rest
