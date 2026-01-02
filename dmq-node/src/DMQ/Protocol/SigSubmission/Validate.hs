{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Encapsulates signature validation utilities leveraged by the mempool writer
--
module DMQ.Protocol.SigSubmission.Validate where

import Control.Monad
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (Exception)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Except.Extra
import Control.Tracer (Tracer, traceWith)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable
import Data.Word

import Cardano.Crypto.DSIGN.Class (ContextDSIGN)
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.KES.Class (KESAlgorithm (..))
import Cardano.KESAgent.KES.Crypto as KES
import Cardano.KESAgent.KES.OCert (OCert (..), OCertSignable, validateOCert)
import Cardano.Ledger.BaseTypes.NonZero
import Cardano.Ledger.Hashes

import DMQ.Diffusion.NodeKernel (PoolValidationCtx (..))
import DMQ.Protocol.SigSubmission.Type
import Ouroboros.Consensus.Shelley.Ledger.Query
import Ouroboros.Network.TxSubmission.Mempool.Simple
import Ouroboros.Network.Util.ShowProxy


-- | The type of non-fatal failures reported by the mempool writer
-- for invalid messages
--
data instance TxValidationFail (Sig crypto) =
    SigInvalid SigValidationError
  | SigDuplicate
  | SigExpired
  | SigResultOther Text
  deriving (Eq, Show)

instance (Typeable crypto) => ShowProxy (TxValidationFail (Sig crypto))

instance (Typeable crypto) => Exception (TxValidationFail (Sig crypto))

instance ToJSON (TxValidationFail (Sig crypto)) where
  toJSON SigDuplicate = String "duplicate"
  toJSON SigExpired   = String "expired"
  toJSON (SigInvalid e) = object
    [ "type" .= String "invalid"
    , "reason" .= show e
    ]
  toJSON (SigResultOther txt) = object
    [ "type" .= String "other"
    , "reason" .= txt
    ]


data SigValidationError =
    InvalidKESSignature KESPeriod KESPeriod String
  | InvalidSignatureOCERT
      !Word64    -- OCert counter
      !KESPeriod -- OCert KES period
      !String    -- DSIGN error message
  | InvalidOCertCounter
      !Word64 -- last seen
      !Word64 -- received
  | KESBeforeStartOCERT KESPeriod KESPeriod
  | KESAfterEndOCERT KESPeriod KESPeriod
  | UnrecognizedPool
  | NotInitialized
  | ClockSkew
  deriving (Eq, Show)


c_MAX_CLOCK_SKEW_SEC :: NominalDiffTime
c_MAX_CLOCK_SKEW_SEC = 5

pattern NotZeroSetSnapshot :: StakeSnapshot
pattern NotZeroSetSnapshot <- (isZero . ssSetPool -> False)

pattern NotZeroMarkSnapshot :: StakeSnapshot
pattern NotZeroMarkSnapshot <- (isZero . ssMarkPool -> False)

pattern ZeroSetSnapshot :: StakeSnapshot
pattern ZeroSetSnapshot <- (isZero . ssSetPool -> True)

{-# COMPLETE NotZeroSetSnapshot, NotZeroMarkSnapshot, ZeroSetSnapshot #-}


-- TODO:
--  We don't validate ocert numbers, since we might not have necessary
--  information to do so, but we can validate that they are growing.
validateSig :: forall crypto m.
               ( Crypto crypto
               , ContextDSIGN (KES.DSIGN crypto) ~ ()
               , DSIGN.Signable (DSIGN crypto) (OCertSignable crypto)
               , ContextKES (KES crypto) ~ ()
               , Signable (KES crypto) ByteString
               , MonadSTM m
               )
            => Tracer m (SigId, TxValidationFail (Sig crypto))
            -> (DSIGN.VerKeyDSIGN (DSIGN crypto) -> KeyHash StakePool)
            -> [Sig crypto]
            -> PoolValidationCtx m
            -- ^ cardano pool id verification
            -> ExceptT (Sig crypto, TxValidationFail (Sig crypto)) m
                       [(Sig crypto, Either (TxValidationFail (Sig crypto)) ())]
validateSig tracer verKeyHashingFn sigs ctx = traverse process' sigs
  where
    DMQPoolValidationCtx now mNextEpoch pools ocertCountersVar = ctx

    process' sig =
      let result = process sig
      in bimapExceptT (sig,) (sig,) $
           result `catchLeftT` \e -> result <* lift (traceWith tracer (sigId sig, e))

    process sig@Sig { sigSignedBytes = signedBytes,
                      sigKESPeriod,
                      sigOpCertificate = SigOpCertificate ocert@OCert {
                        ocertKESPeriod,
                        ocertVkHot,
                        ocertN
                        },
                      sigColdKey = SigColdKey coldKey,
                      sigKESSignature = SigKESSignature kesSig
                } = do
      sigKESPeriod < endKESPeriod
         ?! KESAfterEndOCERT endKESPeriod sigKESPeriod
      sigKESPeriod >= startKESPeriod
         ?! KESBeforeStartOCERT startKESPeriod sigKESPeriod
      e <- case Map.lookup (verKeyHashingFn coldKey) pools of
        Nothing | isNothing mNextEpoch
                  -> right . Left . SigResultOther $ Text.pack "not initialized yet"
                | otherwise
                  -> left $ SigInvalid UnrecognizedPool
        Just ss | NotZeroSetSnapshot <- ss ->
                    if | now < nextEpoch -> success
                         -- localstatequery is late, but the pool is about to expire
                       | isZero (ssMarkPool ss)
                       , now > addUTCTime c_MAX_CLOCK_SKEW_SEC nextEpoch -> left SigExpired
                         -- we bound the time we're willing to approve a message
                         -- in case smth happened to localstatequery and it's taking
                         -- too long to update our state
                       | now <= addUTCTime c_MAX_CLOCK_SKEW_SEC nextEpoch -> success
                       | otherwise -> right . Left $ SigInvalid ClockSkew
                | NotZeroMarkSnapshot <- ss ->
                    -- we take abs time in case we're late with our own
                    -- localstatequery update, and/or the other side's clock
                    -- is ahead, and we're just about or have just crossed the epoch
                    -- and the pool is expected to move into the set mark
                    if | abs (diffUTCTime nextEpoch now) <= c_MAX_CLOCK_SKEW_SEC -> success
                       | diffUTCTime nextEpoch now > c_MAX_CLOCK_SKEW_SEC ->
                           left . SigResultOther $ Text.pack "pool not eligible yet"
                       | otherwise -> right . Left $ SigInvalid ClockSkew
                  -- pool is deregistered and ineligible to mint blocks
                | ZeroSetSnapshot <- ss ->
                    left SigExpired
          where
            -- mNextEpoch and pools are initialized in one STM transaction
            -- and fromJust will not fail here
            nextEpoch = fromJust mNextEpoch
      -- validate OCert, which includes verifying its signature
      validateOCert coldKey ocertVkHot ocert
         ?!: InvalidSignatureOCERT ocertN sigKESPeriod
      -- validate KES signature of the payload
      verifyKES () ocertVkHot
                (unKESPeriod sigKESPeriod - unKESPeriod startKESPeriod)
                (LBS.toStrict signedBytes)
                kesSig
         ?!: InvalidKESSignature ocertKESPeriod sigKESPeriod
      join . lift . atomically $ stateTVar ocertCountersVar \ocertCounters ->
        let f = \case
              Nothing -> Right $ Just ocertN
              Just n | n <= ocertN -> Right $ Just ocertN
                     | otherwise   -> Left $ InvalidOCertCounter n ocertN
        in case Map.alterF f (verKeyHashingFn coldKey) ocertCounters of
          Right ocertCounters' -> (void success, ocertCounters')
          Left  err            -> (throwE (SigInvalid err), ocertCounters)
      -- for eg. remember to run all results with possibly non-fatal errors
      let result = e
      case result of
        Left e' -> lift $ traceWith tracer (sigId sig, e')
        Right _ -> pure ()
      right result
      where
        success = right $ Right ()

        startKESPeriod, endKESPeriod :: KESPeriod

        startKESPeriod = ocertKESPeriod
        -- TODO: is `totalPeriodsKES` the same as `praosMaxKESEvo`
        -- or `sgMaxKESEvolution` in the genesis file?
        endKESPeriod   = KESPeriod $ unKESPeriod startKESPeriod
                                   + totalPeriodsKES (Proxy :: Proxy (KES crypto))

        (?!:) :: Either e1 ()
              -> (e1 -> SigValidationError)
              -> ExceptT (TxValidationFail (Sig crypto)) m ()
        (?!:) result f = firstExceptT (SigInvalid . f) . hoistEither $ result

        (?!) :: Bool
             -> SigValidationError
             -> ExceptT (TxValidationFail (Sig crypto)) m ()
        (?!) flag sve = if flag then void success else left (SigInvalid sve)

        infix 1 ?!
        infix 1 ?!:
