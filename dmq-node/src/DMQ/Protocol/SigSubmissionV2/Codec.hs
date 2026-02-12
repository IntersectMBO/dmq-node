{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module DMQ.Protocol.SigSubmissionV2.Codec
  ( codecSigSubmissionV2
  , codecSigSubmissionV2Id
  , byteLimitsSigSubmissionV2
  , timeLimitsSigSubmissionV2
  , encodeSigSubmissionV2
  , anncodecSigSubmissionV2
  , anncodecSigSubmissionV2'
  ) where

import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadTime.SI
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Text.Printf

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR

import Network.TypedProtocol.Codec.CBOR
  
import Ouroboros.Network.Protocol.Codec.Utils (WithByteSpan (..))
import Ouroboros.Network.Protocol.Codec.Utils qualified as Utils
import Ouroboros.Network.Protocol.Limits

import Cardano.KESAgent.KES.Crypto (Crypto (..))
  
import DMQ.Protocol.SigSubmissionV2.Type
import DMQ.Protocol.SigSubmission.Codec qualified as V1

-- | Byte Limits.
byteLimitsSigSubmissionV2
  :: forall bytes sigId sig.
     (bytes -> Word)
  -> ProtocolSizeLimits (SigSubmissionV2 sigId sig) bytes
byteLimitsSigSubmissionV2 = ProtocolSizeLimits stateToLimit
  where
    stateToLimit
      :: forall (st :: SigSubmissionV2 sigId sig).
         ActiveState st
      => StateToken st
      -> Word
    stateToLimit (SingSigIds SingBlocking)    = largeByteLimit
    stateToLimit (SingSigIds SingNonBlocking) = largeByteLimit
    stateToLimit SingSigs                     = largeByteLimit
    stateToLimit SingIdle                     = smallByteLimit
    stateToLimit a@SingDone                   = notActiveState a

-- | 'SigSubmissionV2' time limits.
--
-- +---------------------------------+---------------+
-- | 'SigSubmissionV2' state         | timeout (s)   |
-- +=================================+===============+
-- | `StIdle`                        | `waitForever` |
-- +---------------------------------+---------------+
-- | @'StSigIds' 'StBlocking'@       | `Just 20`     |
-- +---------------------------------+---------------+
-- | @'StOSigIds' 'StNonBlocking'@   | `shortWait`   |
-- +---------------------------------+---------------+
-- | `StObjects`                     | `shortWait`   |
-- +---------------------------------+---------------+
timeLimitsSigSubmissionV2
  :: forall (sigId :: Type) (sig :: Type).
     ProtocolTimeLimits (SigSubmissionV2 sigId sig)
timeLimitsSigSubmissionV2 = ProtocolTimeLimits stateToLimit
  where
    stateToLimit
      :: forall (st :: SigSubmissionV2 sigId sig).
         ActiveState st
      => StateToken st
      -> Maybe DiffTime
    stateToLimit (SingSigIds SingBlocking)    = Just 20
    stateToLimit (SingSigIds SingNonBlocking) = shortWait
    stateToLimit SingSigs                     = shortWait
    stateToLimit SingIdle                     = waitForever
    stateToLimit a@SingDone                   = notActiveState a


codecSigSubmissionV2
  :: forall (sigId :: Type) (sig :: Type) m.
     MonadST m
  => (sigId -> CBOR.Encoding)         -- ^ encode `sigId`
  -> (forall s. CBOR.Decoder s sigId) -- ^ decode `sigId`
  -> (sig   -> CBOR.Encoding)         -- ^ encode `sig`
  -> (forall s. CBOR.Decoder s sig)   -- ^ decode `sig`
  -> Codec (SigSubmissionV2 sigId sig) CBOR.DeserialiseFailure m ByteString
codecSigSubmissionV2
  encodeSigId decodeSigId
  encodeSig   decodeSig
  =
  mkCodecCborLazyBS
    (encodeSigSubmissionV2 encodeSigId encodeSig)
    decode
    where
      decode
        :: forall (st :: SigSubmissionV2 sigId sig).
           ActiveState st
        => StateToken st
        -> forall s. CBOR.Decoder s (SomeMessage st)
      decode stok = do
        len <- CBOR.decodeListLen
        key <- CBOR.decodeWord
        decodeSigSubmissionV2 decodeSigId decodeSig stok len key


encodeSigSubmissionV2
  :: forall (sigId :: Type) (sig :: Type)
            (st  :: SigSubmissionV2 sigId sig)
            (st' :: SigSubmissionV2 sigId sig).
     (sigId -> CBOR.Encoding) -- ^ encode 'sigId'
  -> (sig   -> CBOR.Encoding) -- ^ encode 'sig'
  -> Message (SigSubmissionV2 sigId sig) st st'
  -> CBOR.Encoding
encodeSigSubmissionV2 encodeObjectId encodeObject = encode
  where
    encode
      :: forall st0 st1.
         Message (SigSubmissionV2 sigId sig) st0 st1
      -> CBOR.Encoding
    encode (MsgRequestSigIds blocking (NumIdsAck ackNo) (NumIdsReq reqNo)) =
         CBOR.encodeListLen 4
      <> CBOR.encodeWord 1
      <> CBOR.encodeBool
           ( case blocking of
               SingBlocking    -> True
               SingNonBlocking -> False
           )
      <> CBOR.encodeWord16 ackNo
      <> CBOR.encodeWord16 reqNo

    encode (MsgReplySigIds objIds) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 2
      <> CBOR.encodeListLenIndef
      <> foldr (\(sigid, SizeInBytes sz) r ->
                       CBOR.encodeListLen 2
                    <> encodeObjectId sigid
                    <> CBOR.encodeWord32 sz
                    <> r) CBOR.encodeBreak objIds

    encode MsgReplyNoSigIds =
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 3

    encode (MsgRequestSigs objIds) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 4
      <> CBOR.encodeListLenIndef
      <> foldMap encodeObjectId objIds
      <> CBOR.encodeBreak

    encode (MsgReplySigs objects) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 5
      <> CBOR.encodeListLenIndef
      <> foldMap encodeObject objects
      <> CBOR.encodeBreak

    encode MsgDone =
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 6


decodeSigSubmissionV2
  :: forall (sigId :: Type) (sig :: Type)
            (st :: SigSubmissionV2 sigId sig) s.
     ActiveState st
  => (forall s'. CBOR.Decoder s' sigId) -- ^ decode 'sigId'
  -> (forall s'. CBOR.Decoder s' sig)   -- ^ decode sig
  -> StateToken st
  -> Int
  -> Word
  -> CBOR.Decoder s (SomeMessage st)
decodeSigSubmissionV2 decodeSigId decodeSig = decode
  where
    decode
      :: forall (st' :: SigSubmissionV2 sigId sig).
         ActiveState st'
      => StateToken st'
      -> Int
      -> Word
      -> CBOR.Decoder s (SomeMessage st')
    decode stok len key = do
      case (stok, len, key) of
        (SingIdle, 4, 1) -> do
          blocking <- CBOR.decodeBool
          ackNo <- NumIdsAck <$> CBOR.decodeWord16
          reqNo <- NumIdsReq <$> CBOR.decodeWord16
          return $! if blocking
            then SomeMessage $ MsgRequestSigIds SingBlocking ackNo reqNo
            else SomeMessage $ MsgRequestSigIds SingNonBlocking ackNo reqNo

        (SingSigIds b, 2, 2) -> do
          CBOR.decodeListLenIndef
          sigIds <- CBOR.decodeSequenceLenIndef
                      (flip (:))
                      []
                      reverse
                      (do CBOR.decodeListLenOf 2
                          sigid <- decodeSigId
                          sz   <- CBOR.decodeWord32
                          return (sigid, SizeInBytes sz))
          case (b, sigIds) of
            (SingBlocking, t : ts) ->
              return
                $ SomeMessage
                $ MsgReplySigIds (BlockingReply (t NonEmpty.:| ts))

            (SingNonBlocking, ts) ->
              return
                $ SomeMessage
                $ MsgReplySigIds (NonBlockingReply ts)

            (SingBlocking, []) ->
              fail "codecSigSubmissionV2: MsgReplySigIds: empty list not permitted"

        (SingSigIds SingBlocking, 1, 3) ->
          return (SomeMessage MsgReplyNoSigIds)

        (SingIdle, 2, 4) -> do
          CBOR.decodeListLenIndef
          sigIds <- CBOR.decodeSequenceLenIndef
                      (flip (:))
                      []
                      reverse
                      decodeSigId
          return $ SomeMessage $ MsgRequestSigs sigIds

        (SingSigs, 2, 5) -> do
          CBOR.decodeListLenIndef
          sigs <- CBOR.decodeSequenceLenIndef
                      (flip (:))
                      []
                      reverse
                      decodeSig
          return $ SomeMessage $ MsgReplySigs sigs

        (SingIdle, 1, 6) ->
          return $ SomeMessage MsgDone

        (SingDone, _, _) -> notActiveState stok

        -- failures
        (_, _, _) ->
          fail $ printf "codecSigSubmissionV2 (%s) unexpected key %d, length %d" (show stok) key len


codecSigSubmissionV2Id
  :: forall sigId sig m.
     Monad m
  => Codec
       (SigSubmissionV2 sigId sig)
       CodecFailure
       m
       (AnyMessage (SigSubmissionV2 sigId sig))
codecSigSubmissionV2Id = Codec {encode, decode}
  where
    encode
      :: forall st st'.
         ( ActiveState st
         , StateTokenI st
         )
      => Message (SigSubmissionV2 sigId sig) st st'
      -> AnyMessage (SigSubmissionV2 sigId sig)
    encode = AnyMessage

    decode
      :: forall (st :: SigSubmissionV2 sigId sig).
         ActiveState st
      => StateToken st
      -> m (DecodeStep
             (AnyMessage (SigSubmissionV2 sigId sig))
             CodecFailure
             m
             (SomeMessage st)
           )
    decode stok = return $ DecodePartial $ \bytes ->
      return $ case (stok, bytes) of
        (SingIdle, Just (AnyMessage msg@(MsgRequestSigIds SingBlocking _ _))) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingIdle, Just (AnyMessage msg@(MsgRequestSigIds SingNonBlocking _ _))) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingIdle, Just (AnyMessage msg@(MsgRequestSigs {}))) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingSigs, Just (AnyMessage msg@(MsgReplySigs {}))) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingSigIds b, Just (AnyMessage msg)) -> case (b, msg) of
          (SingBlocking, MsgReplySigIds (BlockingReply {})) ->
            DecodeDone (SomeMessage msg) Nothing
          (SingBlocking, MsgReplyNoSigIds) ->
            DecodeDone (SomeMessage msg) Nothing
          (SingNonBlocking, MsgReplySigIds (NonBlockingReply {})) ->
            DecodeDone (SomeMessage msg) Nothing
          (_, _) ->
            DecodeFail $ CodecFailure "codecSigSubmissionV2Id: no matching message"
        (SingIdle, Just (AnyMessage msg@MsgDone)) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingDone, _) ->
          notActiveState stok
        (_, _) ->
          DecodeFail $ CodecFailure "codecSigSubmissionV2Id: no matching message"


-- | An 'AnnotatedCodec' with a custom `sigWithBytes` wrapper of `sig`,
-- e.g. `sigWithBytes ~ WithBytes sig`.
--
anncodecSigSubmissionV2
  :: forall (sigId :: Type) (sig :: Type) (sigWithBytes :: Type) m.
     MonadST m
  => (ByteString -> sig -> sigWithBytes)
  -- ^ `withBytes` constructor
  -> (sigId -> CBOR.Encoding)
  -- ^ encode 'sigid'
  -> (forall s . CBOR.Decoder s sigId)
  -- ^ decode 'sigid'
  -> (sigWithBytes -> CBOR.Encoding)
  -- ^ encode `sig`
  -> (forall s . CBOR.Decoder s (ByteString -> sig))
  -- ^ decode signature
  -> AnnotatedCodec (SigSubmissionV2 sigId sigWithBytes) CBOR.DeserialiseFailure m ByteString
anncodecSigSubmissionV2 mkWithBytes encodeSigId decodeSigId
                                    encodeSig   decodeSig =
    mkCodecCborLazyBS
      (encodeSigSubmissionV2 encodeSigId encodeSig)
      decode
  where
    decode :: forall (st :: SigSubmissionV2 sigId sigWithBytes).
              ActiveState st
           => StateToken st
           -> forall s. CBOR.Decoder s (Annotator ByteString st)
    decode =
      decodeSigSubmissionV2' @sig
                             @sigWithBytes
                             @WithByteSpan
                             @ByteString
                             mkWithBytes'
                             decodeSigId
                             (Utils.decodeWithByteSpan decodeSig)
  
    mkWithBytes' :: ByteString
                 -> WithByteSpan (ByteString -> sig)
                 -> sigWithBytes
    mkWithBytes' bytes (WithByteSpan (fn, start, end)) =
        mkWithBytes (Utils.bytesBetweenOffsets start end bytes) -- bytes of the transaction
                    (fn bytes) -- note: fn expects full bytes


decodeSigSubmissionV2'
  :: forall (sig          :: Type)
            (sigWithBytes :: Type)
            (withByteSpan :: Type -> Type)
            (bytes        :: Type)
            (sigId        :: Type)
            (st           :: SigSubmissionV2 sigId sigWithBytes)
            s.
     ActiveState st
  => (bytes -> withByteSpan (bytes -> sig) -> sigWithBytes)
  -> (forall s'. CBOR.Decoder s' sigId) -- ^ decode 'sigId'
  -> (forall s'. CBOR.Decoder s' (withByteSpan (bytes -> sig)))
  -> StateToken st
  -> CBOR.Decoder s (Annotator bytes st)
decodeSigSubmissionV2' mkWithBytes decodeSigId decodeSig sok = do
  len <- CBOR.decodeListLen
  key <- CBOR.decodeWord
  decode sok len key
  where
    decode stok len key = do
      case (stok, len, key) of
        (SingIdle, 4, 1) -> do
          blocking <- CBOR.decodeBool
          ackNo <- NumIdsAck <$> CBOR.decodeWord16
          reqNo <- NumIdsReq <$> CBOR.decodeWord16
          return $! if blocking
            then Annotator $ \_ -> SomeMessage $ MsgRequestSigIds SingBlocking ackNo reqNo
            else Annotator $ \_ -> SomeMessage $ MsgRequestSigIds SingNonBlocking ackNo reqNo

        (SingSigIds b, 2, 2) -> do
          CBOR.decodeListLenIndef
          sigIds <- CBOR.decodeSequenceLenIndef
                      (flip (:))
                      []
                      reverse
                      (do CBOR.decodeListLenOf 2
                          sigid <- decodeSigId
                          sz   <- CBOR.decodeWord32
                          return (sigid, SizeInBytes sz))
          case (b, sigIds) of
            (SingBlocking, t : ts) ->
              return
                $ Annotator $ \_ -> SomeMessage $ MsgReplySigIds (BlockingReply (t NonEmpty.:| ts))

            (SingNonBlocking, ts) ->
              return
                $ Annotator $ \_ -> SomeMessage $ MsgReplySigIds (NonBlockingReply ts)

            (SingBlocking, []) ->
              fail "codecSigSubmissionV2: MsgReplySigIds: empty list not permitted"

        (SingSigIds SingBlocking, 1, 3) ->
          return (Annotator $ \_ -> SomeMessage MsgReplyNoSigIds)

        (SingIdle, 2, 4) -> do
          CBOR.decodeListLenIndef
          sigIds <- CBOR.decodeSequenceLenIndef
                      (flip (:))
                      []
                      reverse
                      decodeSigId
          return $ Annotator $ \_ -> SomeMessage $ MsgRequestSigs sigIds

        (SingSigs, 2, 5) -> do
          CBOR.decodeListLenIndef
          sigs <- CBOR.decodeSequenceLenIndef
                      (flip (:))
                      []
                      reverse
                      decodeSig
          return (Annotator $ \bytes -> SomeMessage (MsgReplySigs $ mkWithBytes bytes <$> sigs))

        (SingIdle, 1, 6) ->
          return $ Annotator $ \_ -> SomeMessage MsgDone

        (SingDone, _, _) -> notActiveState stok

        -- failures
        (_, _, _) ->
          fail $ printf "codecSigSubmissionV2 (%s) unexpected key %d, length %d" (show stok) key len


anncodecSigSubmissionV2'
  :: forall crypto m.
     ( Crypto crypto
     , MonadST m
     )
  => AnnotatedCodec (SigSubmissionV2 SigId (Sig crypto)) CBOR.DeserialiseFailure m ByteString
anncodecSigSubmissionV2' =
  anncodecSigSubmissionV2
    SigWithBytes
    V1.encodeSigId V1.decodeSigId
    V1.encodeSig   V1.decodeSig
