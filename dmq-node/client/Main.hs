{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TypeApplications         #-}

-- | Client for a dmq-node's node-to-client socket.
--
-- `submit` signs a message with a pool's KES key and submits it through
-- LocalMsgSubmission; `notify` reads messages through LocalMsgNotification.
--
-- The message is written from the CDDL (`cddl/specs/sig.cddl`) field by
-- field, and the `Sig` that is submitted is the library's decoding of those
-- bytes, so it never serialises a `SigRaw`.
module Main (main) where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.DeepSeq (NFData)
import Control.Exception (throwIO)
import Control.Monad (unless)
import "contra-tracer" Control.Tracer (nullTracer)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS.Char8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS.Char8
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Void (Void, absurd)
import Data.Word (Word32)
import Options.Applicative
import System.Exit (die)
import System.IO (hFlush, stdout)
import System.IOManager (withIOManager)

import Cardano.Binary (decodeFullDecoder)
import Cardano.Binary.FixedSizeCodec (decodeFixedSized, encodeFixedSized)
import Cardano.Crypto.DSIGN.Class (VerKeyDSIGN)
import Cardano.Crypto.Hash.Class (castHash, hashToBytesAsHex, hashWith)
import Cardano.Crypto.KES.Class (SignKeyKES, SignKeyWithPeriodKES (..),
           forgetSignKeyKES, rawDeserialiseSignKeyKES, signKES)
import Cardano.Crypto.Libsodium (sodiumInit)
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.Evolution (updateKESTo)
import Cardano.KESAgent.KES.OCert (OCert (..))
import Cardano.KESAgent.Protocols.StandardCrypto (StandardCrypto)

import Network.Mux qualified as Mx

import Ouroboros.Network.ConnectionManager.Types (DataFlow (..))
import Ouroboros.Network.Driver.Simple (runAnnotatedPeer)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux qualified as Mx
import Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Version (simpleSingletonVersions)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client qualified as Submission
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import Ouroboros.Network.Snocket (localAddressFromPath, localSnocket,
           makeLocalBearer)
import Ouroboros.Network.Socket (ConnectToArgs (..), HandshakeCallbacks (..),
           connectToNode, nullNetworkConnectTracers)

import DMQ.NodeToClient (localMsgNotificationMiniProtocolNum,
           localMsgSubmissionMiniProtocolNum, ntcHandshakeArguments)
import DMQ.NodeToClient.Version (NodeToClientVersion (..), stdVersionDataNTC)
import DMQ.Policy qualified as Policy
import DMQ.Protocol.LocalMsgNotification.Client qualified as Notification
import DMQ.Protocol.LocalMsgNotification.Codec (codecLocalMsgNotification)
import DMQ.Protocol.LocalMsgSubmission.Client
import DMQ.Protocol.LocalMsgSubmission.Codec (codecLocalMsgSubmission)
import DMQ.Protocol.SigSubmission.Codec (decodeSig, decodeSigOpCertificate,
           encodeSigId, encodeSigOpCertificate, encodeSigPayload)
import DMQ.Protocol.SigSubmission.Type

type C = StandardCrypto

-- | The dmq-node to talk to, and what to do there.
data Command = Command
  { socketPath   :: FilePath
  , networkMagic :: NetworkMagic
  , task         :: Task
  }

data Task = Submit SubmitArgs | Notify Positive

data SubmitArgs = SubmitArgs
  { kesSigningKey :: FilePath
  , opCert        :: FilePath
  , kesPeriod     :: KESPeriod
  , expiresAt     :: Word32
  , body          :: FilePath
  }

-- | Constructed only by the `positive` reader.
newtype Positive = Positive Int

main :: IO ()
main = do
  -- KES signing keys live in libsodium's mlocked memory.
  sodiumInit
  Command { socketPath, networkMagic, task } <-
    execParser (info (commandParser <**> helper) fullDesc)
  case task of
    Submit args -> submit socketPath networkMagic args
    Notify n    -> notify socketPath networkMagic n

-- | The node options follow the subcommand: `dmq-client submit --socket …`.
commandParser :: Parser Command
commandParser = hsubparser
  (  command "submit" (info (withNode (Submit <$> submitParser))
       (progDesc "Sign a message with a pool's KES key and submit it"))
  <> command "notify" (info (withNode (Notify <$> option positive (long "count" <> metavar "N")))
       (progDesc "Print messages as JSON lines: {\"id\", \"bytes\"}"))
  )
  where
    withNode taskParser = Command
      <$> strOption (long "socket" <> metavar "PATH")
      <*> (NetworkMagic <$> option auto (long "network-magic" <> metavar "WORD32"))
      <*> taskParser

    submitParser = SubmitArgs
      <$> strOption (long "kes-signing-key" <> metavar "PATH")
      <*> strOption (long "op-cert" <> metavar "PATH")
      <*> (KESPeriod <$> option auto (long "kes-period" <> metavar "WORD"))
      <*> option auto (long "expires-at" <> metavar "POSIX_SECONDS")
      <*> strOption (long "body" <> metavar "PATH")

    positive = auto >>= \n ->
      if n >= 1 then pure (Positive n)
                else readerError ("--count must be at least 1, got " <> show n)

failWith :: String -> IO a
failWith = die . ("dmq-client: " <>)

--
-- submit
--

submit :: FilePath -> NetworkMagic -> SubmitArgs -> IO ()
submit socketPath networkMagic args = do
  (ocert, coldKey) <- readOpCert (opCert args)
  kesKey <- readKESSigningKey (kesSigningKey args)
  msgBody <- BS.readFile (body args)

  let KESPeriod start = ocertKESPeriod ocert
      KESPeriod current = kesPeriod args
  unless (current >= start) $
    failWith ("--kes-period " <> show current <> " is before the op cert's start period " <> show start)
  SignKeyWithPeriodKES evolvedKey evolution <-
    maybe (failWith ("the KES key cannot evolve to period " <> show current)) pure
      =<< updateKESTo () (kesPeriod args) ocert (SignKeyWithPeriodKES kesKey 0)

  let signedBytes = CBOR.toStrictByteString $
        encodeSigPayload (SigBody msgBody) (kesPeriod args) (fromIntegral (expiresAt args))
      msgId = SigId (castHash (hashWith id signedBytes))
  kesSig <- signKES () evolution signedBytes evolvedKey
  forgetSignKeyKES evolvedKey

  let wireBytes = CBOR.toLazyByteString $
           CBOR.encodeListLen 5
        <> encodeSigId msgId
        <> CBOR.encodePreEncoded signedBytes
        <> encodeFixedSized kesSig
        <> encodeSigOpCertificate (SigOpCertificate ocert)
        <> encodeFixedSized coldKey
  sig <- either (\err -> failWith ("decoding the message we built: " <> show err)) pure
           (decodeMessage wireBytes)

  result <- connect socketPath networkMagic localMsgSubmissionMiniProtocolNum $ \channel ->
    runAnnotatedPeer nullTracer (codecLocalMsgSubmission @C) channel
      (localMsgSubmissionClientPeer
        (Submission.LocalTxSubmissionClient
          (pure (Submission.SendMsgSubmitTx sig (pure . Submission.SendMsgDone . rejection)))))
  case result of
    Nothing  -> BS.Char8.putStrLn (hashToBytesAsHex (getSigId (sigId sig)))
    Just err -> failWith ("rejected: " <> err)
  where
    -- `SigValidationError` has no `NFData`, which the driver needs for the result.
    rejection :: SubmitResult SigValidationError -> Maybe String
    rejection SubmitSuccess    = Nothing
    rejection (SubmitFail err) = Just (show err)

    decodeMessage bytes =
      (\withBytes -> SigWithBytes bytes (withBytes bytes))
        <$> decodeFullDecoder "Sig" (decodeSig @C) bytes

--
-- notify
--

notify :: FilePath -> NetworkMagic -> Positive -> IO ()
notify socketPath networkMagic (Positive n) =
  connect socketPath networkMagic localMsgNotificationMiniProtocolNum $ \channel ->
    runAnnotatedPeer nullTracer (codecLocalMsgNotification @C) channel
      (Notification.localMsgNotificationClientPeer
        (Notification.LocalMsgNotificationClient (pure (request n))))
  where
    request remaining = Notification.SendMsgRequestBlocking $ \msgs _hasMore -> do
      let printed = take remaining (NonEmpty.toList msgs)
      for_ printed $ \sig ->
        LBS.Char8.putStrLn . Aeson.encode $ Aeson.object
          [ "id"    .= sigId sig
          , "bytes" .= Text.decodeUtf8 (Base16.encode (LBS.toStrict (sigBytes sig)))
          ]
      hFlush stdout
      let remaining' = remaining - length printed
      pure $ if remaining' == 0 then Notification.SendMsgDone (pure ()) else request remaining'

--
-- shared
--

-- | Run one initiator mini-protocol over a node-to-client connection, with
-- the same handshake as dmq-node's own node-to-client server.
connect :: NFData a
        => FilePath
        -> NetworkMagic
        -> Mx.MiniProtocolNum
        -> (Mx.Channel IO LBS.ByteString -> IO (a, Maybe LBS.ByteString))
        -> IO a
connect path magic protocolNum run = withIOManager $ \iocp -> do
  result <- connectToNode
    (localSnocket iocp)
    makeLocalBearer
    ConnectToArgs
      { ctaHandshakeCodec      = haHandshakeCodec
      , ctaHandshakeTimeLimits = haTimeLimits
      , ctaVersionDataCodec    = haVersionDataCodec
      , ctaConnectTracers      = nullNetworkConnectTracers
      , ctaHandshakeCallbacks  = HandshakeCallbacks haAcceptVersion haQueryVersion
      }
    (\_ -> pure ())
    (simpleSingletonVersions NodeToClientV_1 (stdVersionDataNTC magic) $ \_ ->
      Mx.OuroborosApplication
        [ Mx.MiniProtocol
            { Mx.miniProtocolNum    = protocolNum
            , Mx.miniProtocolStart  = Mx.StartEagerly
            , Mx.miniProtocolLimits = Policy.localProtocolLimits
            , Mx.miniProtocolRun    = Mx.InitiatorProtocolOnly (Mx.MiniProtocolCb (\_ctx -> run))
            }
        ])
    (const Unidirectional)
    Nothing
    (localAddressFromPath path)
  case result of
    Left err        -> throwIO err
    Right (Left a)  -> pure a
    Right (Right v) -> absurd (v :: Void)
  where
    HandshakeArguments { haHandshakeCodec, haTimeLimits, haVersionDataCodec,
                         haAcceptVersion, haQueryVersion } =
      ntcHandshakeArguments nullTracer

--
-- cardano-cli text envelopes
--

data TextEnvelope = TextEnvelope { envelopeType :: Text, cborHex :: Text }

instance Aeson.FromJSON TextEnvelope where
  parseJSON = Aeson.withObject "TextEnvelope" $ \o ->
    TextEnvelope <$> o .: "type" <*> o .: "cborHex"

readEnvelope :: Text -> FilePath -> IO BS.ByteString
readEnvelope expected path = do
  TextEnvelope { envelopeType, cborHex } <-
    either (\err -> failWith (path <> ": " <> err)) pure
      =<< Aeson.eitherDecodeFileStrict path
  unless (envelopeType == expected) $
    failWith (path <> ": expected type " <> show expected <> ", got " <> show envelopeType)
  either (\err -> failWith (path <> ": cborHex: " <> err)) pure
    (Base16.decode (Text.encodeUtf8 cborHex))

decodeEnvelope :: FilePath -> (forall s. CBOR.Decoder s a) -> BS.ByteString -> IO a
decodeEnvelope path decoder bytes =
  either (\err -> failWith (path <> ": " <> show err)) pure
    (decodeFullDecoder "cborHex" decoder (LBS.fromStrict bytes))

readKESSigningKey :: FilePath -> IO (SignKeyKES (KES C))
readKESSigningKey path = do
  raw <- decodeEnvelope path CBOR.decodeBytes
     =<< readEnvelope "KesSigningKey_ed25519_kes_2^6" path
  maybe (failWith (path <> ": not a Sum6KES signing key")) pure
    =<< rawDeserialiseSignKeyKES raw

-- | A cardano-cli op cert is `[ocert, cold verification key]`.
readOpCert :: FilePath -> IO (OCert C, VerKeyDSIGN (DSIGN C))
readOpCert path =
  decodeEnvelope path decoder =<< readEnvelope "NodeOperationalCertificate" path
  where
    decoder :: CBOR.Decoder s (OCert C, VerKeyDSIGN (DSIGN C))
    decoder = do
      CBOR.decodeListLenOf 2
      SigOpCertificate ocert <- decodeSigOpCertificate
      coldKey <- decodeFixedSized
      pure (ocert, coldKey)
