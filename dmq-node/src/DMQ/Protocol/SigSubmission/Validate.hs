{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Encapsulates signature validation utilities leveraged by the mempool writer
--
module DMQ.Protocol.SigSubmission.Validate
  ( validateSig
  , SigValidationException (..)
  , SigValidationError (..)
  , SigValidationTrace (..)
  ) where

import Control.Monad.Except (Except)
import Control.Monad.Except qualified as Except
import Control.Monad.State.Strict (State, StateT (..))
import Control.Monad.State.Strict qualified as State

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.Hash.Class (castHash, hashWith)
import Cardano.Crypto.KES.Class (KESAlgorithm (..))
import Cardano.KESAgent.KES.Crypto as KES
import Cardano.KESAgent.KES.OCert (OCert (..), OCertSignable, validateOCert)
-- NOTE: one should be careful with `ssMarkPool` in this module, so it is not
-- imported, see a note in
-- `DMQ.NodeToClient.LocalStateQueryClient.cardanoClient`.
import Cardano.Ledger.Api.State.Query (StakeSnapshot (ssSetPool))
import Cardano.Ledger.BaseTypes.NonZero qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger

import DMQ.Diffusion.NodeKernel (PoolValidationCtx (..), Readiness (..))
import DMQ.Protocol.SigSubmission.Type


pattern NotZeroSetSnapshot :: StakeSnapshot
pattern NotZeroSetSnapshot <- (Ledger.isZero . ssSetPool -> False)

pattern ZeroSetSnapshot :: StakeSnapshot
pattern ZeroSetSnapshot <- (Ledger.isZero . ssSetPool -> True)

{-# COMPLETE NotZeroSetSnapshot, ZeroSetSnapshot #-}


validateSigId :: Sig crypto -> Bool
validateSigId Sig { sigId = SigId hash, sigSignedBytes } =
  hash
  ==
  castHash (hashWith id (LBS.toStrict sigSignedBytes))


validateSig :: forall crypto.
               ( Crypto crypto
               , DSIGN crypto ~ Ledger.DSIGN
               , DSIGN.Signable (DSIGN crypto) (OCertSignable crypto)
               , ContextKES (KES crypto) ~ ()
               , Signable (KES crypto) ByteString
               )
            => [Sig crypto]
            -> PoolValidationCtx
            -> ([Either (SigId, SigValidationError) (Sig crypto)], PoolValidationCtx)
validateSig sigs ctx0@PoolValidationCtx { vctxNow = now, vctxPraosMaxKESEvo = maxKESEvo } =
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
                       sigKESSignature = SigKESSignature kesSig,
                       sigExpiresAt
                     } = do
      -- check if sig expired
      utcTimeToPOSIXSeconds now <= sigExpiresAt ?! SigExpired

      -- TODO: if new cborg version is released, validation of SigId should be
      -- moved to the decoder, right now the decoder only verifies that we
      -- received the right amount of bytes.
      validateSigId sig ?! InvalidSigId

      ctx@PoolValidationCtx { vctxReadiness, vctxStakeMap, vctxOcertMap } <- State.get

      --
      -- verify KES period
      --

      sigKESPeriod < endKESPeriod    ?! KESAfterEndOCERT endKESPeriod sigKESPeriod
      sigKESPeriod >= startKESPeriod ?! KESBeforeStartOCERT startKESPeriod sigKESPeriod

      --
      -- verify that the pool is registered and eligible to mint blocks
      --

      case Map.lookup (Ledger.hashKey (Ledger.VKey coldKey)) vctxStakeMap of
        Nothing ->
          left $ case vctxReadiness of
            Ready    -> UnrecognizedPool
            NotReady -> NotInitialized

        Just NotZeroSetSnapshot -> return ()

        -- pool unregistered and is ineligible to mint signatures
        Just ZeroSetSnapshot -> left PoolNotEligible

      --
      -- verify that our observations of ocertN are strictly monotonic
      --

      case Map.alterF (\a -> (a, Just ocertN))
                      (Ledger.hashKey (Ledger.VKey coldKey))
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
        endKESPeriod   = KESPeriod $ unKESPeriod startKESPeriod
                                   + fromIntegral maxKESEvo

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
