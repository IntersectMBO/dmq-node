module DMQ.Protocol.SigSubmissionV2.Codec.CDDL where

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise.Class qualified as Serialise
import Data.ByteString.Lazy qualified as BL

import Network.TypedProtocol.Codec

import DMQ.Protocol.SigSubmissionV2.Codec
import DMQ.Protocol.SigSubmissionV2.Test (Sig, SigId)
import DMQ.Protocol.SigSubmissionV2.Type hiding (Sig, SigId)


sigSubmissionV2Codec :: Codec (SigSubmissionV2 SigId Sig)
                              CBOR.DeserialiseFailure IO BL.ByteString
sigSubmissionV2Codec =
    codecSigSubmissionV2
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode
