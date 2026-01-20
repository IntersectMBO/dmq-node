{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.DMQ.SigSubmission.Types
  ( Sig (..)
  , SigId
  , Mempool
  , emptyMempool
  , newMempool
  , readMempool
  , getMempoolReader
  , getMempoolWriter
  , maxSigSize
  , LargeNonEmptyList (..)
  , SimResults (..)
  , WithThreadAndTime (..)
  , sigSubmissionCodec2
  , evaluateTrace
  , verboseTracer
  , debugTracer
) where

import Prelude hiding (seq)

import NoThunks.Class

import Control.Concurrent.Class.MonadSTM
import Control.DeepSeq
import Control.Exception (SomeException (..))
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.IOSim hiding (SimResult)
import Control.Tracer (Tracer (..), showTracing, traceWith)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR

import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)

import Network.TypedProtocol.Codec

import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V1 (TxSubmissionMempoolWriter)
import Ouroboros.Network.TxSubmission.Mempool.Reader
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool)
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool
import Ouroboros.Network.Util.ShowProxy

import Test.QuickCheck
import Text.Printf
import DMQ.Protocol.SigSubmissionV2.Type (SigSubmissionV2)
import DMQ.Protocol.SigSubmissionV2.Codec (codecSigSubmissionV2)


data Sig sigid = Sig {
    getSigId      :: !sigid,
    getSigSize    :: !SizeInBytes,
    getSigAdvSize :: !SizeInBytes,
    -- | If false this means that when this sig will be submitted to a remote
    -- mempool it will not be valid.  The outbound mempool might contain
    -- invalid sig's in this sense.
    getSigValid   :: !Bool
  }
  deriving (Eq, Ord, Show, Generic, NFData)

instance NoThunks sigid => NoThunks (Sig sigid)
instance ShowProxy sigid => ShowProxy (Sig sigid) where
    showProxy _ = "Sig " ++ showProxy (Proxy :: Proxy sigid)

instance Arbitrary sigid => Arbitrary (Sig sigid) where
    arbitrary = do
      -- note:
      -- generating small sig sizes avoids overflow error when semigroup
      -- instance of `SizeInBytes` is used (summing up all inflight sig
      -- sizes).
      (size, advSize) <- frequency [ (99, (\a -> (a,a)) <$> chooseEnum (0, maxSigSize))
                                   , (1, (,) <$> chooseEnum (0, maxSigSize) <*> chooseEnum (0, maxSigSize))
                                   ]
      Sig <$> arbitrary
         <*> pure size
         <*> pure advSize
         <*> frequency [ (3, pure True)
                       , (1, pure False)
                       ]

-- maximal sig size
maxSigSize :: SizeInBytes
maxSigSize = 65536

type SigId = Int

emptyMempool :: MonadSTM m => m (Mempool m sigid (Sig sigid))
emptyMempool = Mempool.empty

newMempool :: (MonadSTM m, Ord sigid)
           => [Sig sigid] -> m (Mempool m sigid (Sig sigid))
newMempool = Mempool.new getSigId

readMempool :: MonadSTM m => Mempool m sigid (Sig sigid) -> m [Sig sigid]
readMempool  = Mempool.read

getMempoolReader :: forall sigid m.
                    ( MonadSTM m
                    , Ord sigid
                    )
                 => Mempool m sigid (Sig sigid)
                 -> TxSubmissionMempoolReader sigid (Sig sigid) Integer m
getMempoolReader = Mempool.getReader getSigId getSigAdvSize

data InvalidSig = InvalidSig

getMempoolWriter :: forall sigid m.
                    ( MonadSTM m
                    , MonadTime m
                    , Ord sigid
                    )
                 => Mempool m sigid (Sig sigid)
                 -> TxSubmissionMempoolWriter sigid (Sig sigid) Integer m InvalidSig
getMempoolWriter = Mempool.getWriter InvalidSig
                                     getSigId
                                     (\_ sigs ->return
                                                 [ if getSigValid sig
                                                   then Right sig
                                                   else Left (getSigId sig, InvalidSig)
                                                 | sig <- sigs
                                                 ]
                                     )
                                     (\_ -> return ()) 


sigSubmissionCodec2 :: MonadST m
                   => Codec (SigSubmissionV2 SigId (Sig Int))
                            CBOR.DeserialiseFailure m ByteString
sigSubmissionCodec2 =
    codecSigSubmissionV2 CBOR.encodeInt CBOR.decodeInt
                         encodeSig decodeSig
  where
    encodeSig Sig {getSigId, getSigSize, getSigAdvSize, getSigValid} =
         CBOR.encodeListLen 4
      <> CBOR.encodeInt getSigId
      <> CBOR.encodeWord32 (getSizeInBytes getSigSize)
      <> CBOR.encodeWord32 (getSizeInBytes getSigAdvSize)
      <> CBOR.encodeBool getSigValid

    decodeSig = do
      _ <- CBOR.decodeListLen
      Sig <$> CBOR.decodeInt
         <*> (SizeInBytes <$> CBOR.decodeWord32)
         <*> (SizeInBytes <$> CBOR.decodeWord32)
         <*> CBOR.decodeBool


newtype LargeNonEmptyList a = LargeNonEmpty { getLargeNonEmpty :: [a] }
  deriving Show

instance Arbitrary a => Arbitrary (LargeNonEmptyList a) where
    arbitrary =
      LargeNonEmpty <$> suchThat (resize 500 (listOf arbitrary)) ((>25) . length)


-- TODO: Belongs in iosim.
data SimResults a = SimReturn a [String]
                  | SimException SomeException [String]
                  | SimDeadLock [String]

-- Traverses a list of trace events and returns the result along with all log messages.
-- Incase of a pure exception, ie an assert, all tracers evaluated so far are returned.
evaluateTrace :: SimTrace a -> IO (SimResults a)
evaluateTrace = go []
  where
    go as tr = do
      r <- try (evaluate tr)
      case r of
        Right (SimTrace _ _ _ (EventSay s) tr')      -> go (s : as) tr'
        Right (SimTrace _ _ _ _ tr' )                -> go as tr'
        Right (SimPORTrace _ _ _ _ (EventSay s) tr') -> go (s : as) tr'
        Right (SimPORTrace _ _ _ _ _ tr' )           -> go as tr'
        Right (TraceMainReturn _ _ a _)              -> pure $ SimReturn a (reverse as)
        Right (TraceMainException _ _ e _)           -> pure $ SimException e (reverse as)
        Right (TraceDeadlock _ _)                    -> pure $ SimDeadLock (reverse as)
        Right TraceLoop                              -> error "IOSimPOR step time limit exceeded"
        Right (TraceInternalError e)                 -> error ("IOSim: " ++ e)
        Left  (SomeException e)                      -> pure $ SimException (SomeException e) (reverse as)


data WithThreadAndTime a = WithThreadAndTime {
      wtatOccuredAt    :: !Time
    , wtatWithinThread :: !String
    , wtatEvent        :: !a
    }

instance (Show a) => Show (WithThreadAndTime a) where
    show WithThreadAndTime {wtatOccuredAt, wtatWithinThread, wtatEvent} =
        printf "%s: %s: %s" (show wtatOccuredAt) (show wtatWithinThread) (show wtatEvent)

verboseTracer :: forall a m.
                       ( MonadAsync m
                       , MonadSay m
                       , MonadMonotonicTime m
                       , Show a
                       )
               => Tracer m a
verboseTracer = threadAndTimeTracer $ showTracing $ Tracer say

debugTracer :: forall a s. Show a => Tracer (IOSim s) a
debugTracer = threadAndTimeTracer $ showTracing $ Tracer (traceM . show)

threadAndTimeTracer :: forall a m.
                       ( MonadAsync m
                       , MonadMonotonicTime m
                       )
                    => Tracer m (WithThreadAndTime a) -> Tracer m a
threadAndTimeTracer tr = Tracer $ \s -> do
    !now <- getMonotonicTime
    !tid <- myThreadId
    traceWith tr $ WithThreadAndTime now (show tid) s
