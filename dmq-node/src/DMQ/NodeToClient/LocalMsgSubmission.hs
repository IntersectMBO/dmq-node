{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module DMQ.NodeToClient.LocalMsgSubmission where

import Control.Monad.Class.MonadThrow
import Control.Tracer
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Typeable

import DMQ.Protocol.LocalMsgSubmission.Server
import DMQ.Protocol.LocalMsgSubmission.Type

-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localMsgSubmissionServer ::
     forall msgid msg m.
     Monad m
  => (msg -> msgid)
  -- ^ get message id
  -> Tracer m (TraceLocalMsgSubmission msg msgid)
  -> (msg -> m (Either (msgid, SigValidationError) msgid))
  -- ^ add a msg to mempool
  -> m (LocalMsgSubmissionServer msg m ())
localMsgSubmissionServer getMsgId tracer mempoolAddTxs =
    pure server
  where
    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \msg -> do
        traceWith tracer $ TraceReceivedMsg (getMsgId msg)
        res <- mempoolAddTxs msg
        case res of
          Left (msgid, reason) -> do
            traceWith tracer (TraceSubmitFailure msgid reason)
            return (SubmitFail reason, server)
          Right msgid -> do
            traceWith tracer (TraceSubmitAccept msgid)
            return (SubmitSuccess, server)

    , recvMsgDone = ()
    }


data TraceLocalMsgSubmission msg msgid =
    TraceReceivedMsg msgid
  -- ^ A signature was received.
  | TraceSubmitFailure msgid SigValidationError
  -- ^ A signature was rejected with the given validation failure.
  | TraceSubmitAccept msgid
  -- ^ A signature was validated and accepted into the mempool.

deriving instance
     (Show msg, Show msgid)
  => Show (TraceLocalMsgSubmission msg msgid)



data MsgSubmissionServerException msgid msg =
    MsgValidationException msgid SigValidationError
  | TooManyMessages

deriving instance Show msgid
  => Show (MsgSubmissionServerException msgid msg)

instance (Typeable msgid, Typeable msg, Show msgid)
  => Exception (MsgSubmissionServerException msgid msg) where


instance ToJSON msgid
      => ToJSON (TraceLocalMsgSubmission msg msgid) where
  toJSON (TraceReceivedMsg msgid) =
    -- TODO: once we have verbosity levels, we could include the full tx, for
    -- now one can use `TraceSendRecv` tracer for the mini-protocol to see full
    -- msgs.
    object [ "kind" .= Aeson.String "TraceReceivedMsg"
           , "sigId" .= msgid
           ]
  toJSON (TraceSubmitFailure msgid reject) =
    object [ "kind" .= Aeson.String "TraceSubmitFailure"
           , "sigId" .= msgid
           , "reason" .= reject
           ]
  toJSON (TraceSubmitAccept msgid) =
    object [ "kind" .= Aeson.String "TraceSubmitAccept"
           , "sigId" .= msgid
           ]
