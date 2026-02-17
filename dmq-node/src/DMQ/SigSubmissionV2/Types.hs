module DMQ.SigSubmissionV2.Types
  ( SigSubmissionProtocolError (..)
  ) where

import Data.Word (Word16)
import Control.Exception (Exception)
import Control.Monad.Class.MonadThrow (Exception(..))

import DMQ.Protocol.SigSubmissionV2.Type

data SigSubmissionProtocolError =
       ProtocolErrorAckedTooManySigIds
     | ProtocolErrorRequestedNothing
     | ProtocolErrorRequestedTooManySigIds NumIdsReq Word16 NumIdsAck
     | ProtocolErrorRequestBlocking
     | ProtocolErrorRequestNonBlocking
     | ProtocolErrorRequestedUnavailableSig
     | ProtocolErrorSigIdsNotRequested
     | ProtocolErrorSigNotRequested
  deriving Show

instance Exception SigSubmissionProtocolError where
  displayException ProtocolErrorAckedTooManySigIds =
      "The peer tried to acknowledged more sigIds than are available to do so."

  displayException (ProtocolErrorRequestedTooManySigIds reqNo unackedNo maxUnacked) =
      "The peer requested " ++ show reqNo ++ " sigIds which would put the "
   ++ "total in flight over the limit of " ++ show maxUnacked ++ "."
   ++ " Number of unacked sigIds " ++ show unackedNo

  displayException ProtocolErrorRequestedNothing =
      "The peer requested zero sigIds."

  displayException ProtocolErrorRequestBlocking =
      "The peer made a blocking request for more sigIds when there are still "
   ++ "unacknowledged sigIds. It should have used a non-blocking request."

  displayException ProtocolErrorRequestNonBlocking =
      "The peer made a non-blocking request for more sigIds when there are "
   ++ "no unacknowledged sigIds. It should have used a blocking request."

  displayException ProtocolErrorRequestedUnavailableSig =
      "The peer requested a signature which is not available, either "
   ++ "because it was never available or because it was previously requested."

  displayException ProtocolErrorSigIdsNotRequested =
      "The peer replied with more txids than we asked for."

  displayException ProtocolErrorSigNotRequested =
      "The peer replied with a transaction we did not ask for."
