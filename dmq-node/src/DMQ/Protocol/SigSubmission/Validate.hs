{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Encapsulates signature validation utilities leveraged by the mempool writer
--
module DMQ.Protocol.SigSubmission.Validate where

import Control.Exception (Exception (..))
import Control.Monad.Class.MonadTime.SI
import Control.Monad.State.Strict (State, StateT (..))
import Control.Monad.State.Strict qualified as State
import Control.Monad.Except (Except)
import Control.Monad.Except qualified as Except

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
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
import Ouroboros.Network.Util.ShowProxy


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
  | PoolNotEligible
  | UnrecognizedPool
  | NotInitialized
  | ClockSkew
  | SigDuplicate
  | SigExpired
  | SigResultOther Text
  deriving (Eq, Show)

instance ShowProxy SigValidationError where

instance ToJSON SigValidationError where
  toJSON SigDuplicate = String "duplicate"
  toJSON SigExpired   = String "expired"
  toJSON (SigResultOther txt) = object
    [ "type" .= String "other"
    , "reason" .= txt
    ]
  toJSON e = object
    [ "type" .= String "invalid"
    , "reason" .= show e
    ]


data SigValidationException = SigValidationException SigId SigValidationError
  deriving Show

instance Exception SigValidationException


data SigValidationTrace = InvalidSignature SigId SigValidationError
  deriving Show

instance ToJSON SigValidationTrace where
  toJSON (InvalidSignature sigid reason) = object 
    [ "type"   .= String "InvalidSignature"
    , "sigid"  .= sigid
    , "reason" .= reason
    ]


c_MAX_CLOCK_SKEW_SEC :: NominalDiffTime
c_MAX_CLOCK_SKEW_SEC = 5

pattern NotZeroSetSnapshot :: StakeSnapshot
pattern NotZeroSetSnapshot <- (isZero . ssSetPool -> False)

pattern NotZeroMarkSnapshot :: StakeSnapshot
pattern NotZeroMarkSnapshot <- (isZero . ssMarkPool -> False)

pattern ZeroSetSnapshot :: StakeSnapshot
pattern ZeroSetSnapshot <- (isZero . ssSetPool -> True)

{-# COMPLETE NotZeroSetSnapshot, NotZeroMarkSnapshot, ZeroSetSnapshot #-}


validateSig :: forall crypto.
               ( Crypto crypto
               , ContextDSIGN (KES.DSIGN crypto) ~ ()
               , DSIGN.Signable (DSIGN crypto) (OCertSignable crypto)
               , ContextKES (KES crypto) ~ ()
               , Signable (KES crypto) ByteString
               )
            => (DSIGN.VerKeyDSIGN (DSIGN crypto) -> KeyHash StakePool)
            -> UTCTime
            -> [Sig crypto]
            -> PoolValidationCtx
            -> ([Either (SigId, SigValidationError) (Sig crypto)], PoolValidationCtx)
validateSig verKeyHashingFn now sigs ctx0 =
    State.runState (traverse (exceptions . validate) sigs) ctx0
  where
    exceptions :: StateT s (Except e) a
               -> State  s (Either e a)
    exceptions (StateT k) = StateT $ \s ->
      case Except.runExcept (k s) of
        -- in case of a validation error, we continue with un-modified
        -- `PoolValidationCtx`
        Left x        -> return (Left x, s)
        Right (a, s') -> return (Right a, s')

    validate :: Sig crypto
             -> StateT PoolValidationCtx (Except (SigId, SigValidationError)) (Sig crypto)
    validate sig@Sig { sigId,
                       sigSignedBytes = signedBytes,
                       sigKESPeriod,
                       sigOpCertificate = SigOpCertificate ocert@OCert {
                         ocertKESPeriod,
                         ocertVkHot,
                         ocertN
                         },
                       sigColdKey = SigColdKey coldKey,
                       sigKESSignature = SigKESSignature kesSig
                     } = do
      ctx@PoolValidationCtx { vctxEpoch, vctxStakeMap, vctxOcertMap } <- State.get

      --
      -- verify KES period
      --

      sigKESPeriod < endKESPeriod    ?! KESAfterEndOCERT endKESPeriod sigKESPeriod
      sigKESPeriod >= startKESPeriod ?! KESBeforeStartOCERT startKESPeriod sigKESPeriod

      --
      -- verify that the pool is registered and eligible to mint blocks
      --

      let -- `vctxEpoch` and `vctxStakeMap` are initialized in one STM
          -- transaction, which guarantees that fromJust will not fail
          nextEpoch = fromJust vctxEpoch
      case Map.lookup (verKeyHashingFn coldKey) vctxStakeMap of
        Nothing | isNothing vctxEpoch
                  -> left NotInitialized
                | otherwise
                  -> left UnrecognizedPool

        Just ss@NotZeroSetSnapshot ->
          if | now <= addUTCTime c_MAX_CLOCK_SKEW_SEC nextEpoch
             -> return ()

               -- local-state-query is late, but the pool is about to expire
             | isZero (ssMarkPool ss)
             -> left SigExpired

             | otherwise
             -> left ClockSkew

        Just NotZeroMarkSnapshot ->
          -- we take abs time in case we're late with our own local-state-query
          -- update, and/or the other side's clock is ahead, and we're just
          -- about or have just crossed the epoch and the pool is expected to
          -- move into the set mark
          if | abs (diffUTCTime nextEpoch now) <= c_MAX_CLOCK_SKEW_SEC
             -> return ()

             | diffUTCTime nextEpoch now > c_MAX_CLOCK_SKEW_SEC
             -> left PoolNotEligible

             | otherwise
             -> left ClockSkew

        -- pool unregistered and is ineligible to mint blocks
        Just ZeroSetSnapshot -> left SigExpired

      --
      -- verify that our observations of ocertN are strictly monotonic
      --

      case Map.alterF (\a -> (a, Just ocertN))
                      (verKeyHashingFn coldKey)
                      vctxOcertMap of
        (Nothing, ocertCounters')
          -- there is no ocert in the map, e.g. we're validating a signature
          -- produced by that SPO for the first time
          -> State.put ctx { vctxOcertMap = ocertCounters' }
        (Just prevOcertN, ocertCounters')
          -- QUESTION: should we be more strict with `<`!
          | prevOcertN <= ocertN -- `ocertN` is valid
          -> State.put ctx { vctxOcertMap = ocertCounters' }

          | otherwise
          -> left (InvalidOCertCounter prevOcertN ocertN)

      --
      -- Cryptographic checks
      --

      -- validate OCert, which includes verifying its signature
      validateOCert coldKey ocertVkHot ocert
         ?!: InvalidSignatureOCERT ocertN sigKESPeriod

      -- validate KES signature of the payload
      verifyKES () ocertVkHot
                (unKESPeriod sigKESPeriod - unKESPeriod startKESPeriod)
                (LBS.toStrict signedBytes)
                kesSig
         ?!: InvalidKESSignature ocertKESPeriod sigKESPeriod

      return sig
      where
        startKESPeriod, endKESPeriod :: KESPeriod

        startKESPeriod = ocertKESPeriod
        -- TODO: is `totalPeriodsKES` the same as `praosMaxKESEvo`
        -- or `sgMaxKESEvolution` in the genesis file?
        endKESPeriod   = KESPeriod $ unKESPeriod startKESPeriod
                                   + totalPeriodsKES (Proxy :: Proxy (KES crypto))

        (?!:) :: Either err ()
              -> (err -> SigValidationError)
              -> StateT s (Except (SigId, SigValidationError)) ()
        (?!:) Right {} _ = return ()
        (?!:) (Left e) f = Except.throwError (sigId, f e)

        (?!) :: Bool
             -> SigValidationError
             -> StateT s (Except (SigId, SigValidationError)) ()
        (?!) True  _   = return ()
        (?!) False err = Except.throwError (sigId, err)

        infix 1 ?!
        infix 1 ?!:

        left :: SigValidationError -> StateT s (Except (SigId, SigValidationError)) ()
        left err = Except.throwError (sigId, err)
