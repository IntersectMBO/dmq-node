{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module provides the type of LocalMsgProtocol via LocalTxSubmission
--
module DMQ.Protocol.LocalMsgSubmission.Type
  ( module DMQ.Protocol.LocalMsgSubmission.Type
    -- * re-exports
  , SigValidationError (..)
  , module Core
  , module Ouroboros
  ) where

import DMQ.Protocol.SigSubmission.Validate (SigValidationError (..))
import Network.TypedProtocol.Core as Core
import Ouroboros.Network.Protocol.LocalTxSubmission.Type as Ouroboros

-- | The LocalMsgSubmission protocol is an alias for the LocalTxSubmission
--
type LocalMsgSubmission sig = Ouroboros.LocalTxSubmission sig SigValidationError
