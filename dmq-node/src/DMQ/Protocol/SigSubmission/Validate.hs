{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , ValidationCfg (..)
  ) where

import Control.Monad.Class.MonadTime.SI
import Control.Monad.Except (Except)
import Control.Monad.Except qualified as Except
import Control.Monad.State.Strict (State, StateT (..))
import Control.Monad.State.Strict qualified as State

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.OrdPSQ qualified as OrdPSQ
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

import DMQ.Diffusion.NodeKernel (PoolValidationCtx (..), Readiness (..),
           ValidationCfg (..))
import DMQ.Policy qualified as Policy
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
            => ValidationCfg
            -- ^ minimum signature delay produced by the same stake pool
            -> [Sig crypto]
            -> PoolValidationCtx
            -> ([Either (SigId, SigValidationError) (Sig crypto)], PoolValidationCtx)
validateSig ValidationCfg { vcMinSigDelay } sigs = State.runState (traverse (commute . validate) sigs)
  where
    -- Commute `StateT` and `ExceptT` monads, e.g. a natural transformation
    -- `StateT s (Except e) ~> ExceptT e (State s)`. The latter monad allows us
    -- to validate all signatures and retain all errors, rather than fail on
    -- first invalid signature.
    commute :: StateT s (Except e) a
            -> (State s) (Either e a)
            -- equivalent to `ExceptT (State s) a`
    commute (StateT k) = StateT $ \s ->
      case Except.runExcept (k s) of
        -- validation failed, continue with the previous state
        Left x        -> return (Left x, s)
        -- validation succeeded, continue with updated state
        Right (a, s') -> return (Right a, s')

    -- NOTE: we run in `StateT s (Except e)` monad which has the semantics
    -- of `s -> Either e (a, s)`.  In this monad `Except.throwError` abandons
    -- the computation, and ignores the computed state.  Thus we can update the
    -- state even if future validations will prove a signature to be invalid,
    -- and still make sure that invalid signatures do not modify the state.
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
      vctxUTCNow <$> State.get >>= \utcNow -> do
        let posixNow = utcTimeToPOSIXSeconds utcNow
        posixNow <= sigExpiresAt ?! SigExpired

        -- check if sigExpiresAt is not too far in the future
        let posixBound = utcTimeToPOSIXSeconds (Policy.maxSigExpiresAtDelay `addUTCTime` utcNow)
        sigExpiresAt < posixBound ?! SigExpiresAtTooFarInTheFuture

      -- check if the PoolId doesn't forge signatures too frequently
      (\st -> ( vctxNow st
              , vctxLastSigByPoolId st
              ))
        <$> State.get >>= \(now, lastSigByPoolId) -> do
        let fn :: Maybe (Time, ())
               -> (Either DiffTime (), Maybe (Time, ()))
            fn Nothing
                = (Right (), Just (now, ()))

            fn a@(Just (lastSeen, _))
                | sinceLast >= vcMinSigDelay
                = (Right (), Just (now, ()))

                | otherwise
                = (Left sinceLast, a)
              where
                sinceLast = now `diffTime` lastSeen

            (r, lastSigByPoolId') = OrdPSQ.alter fn poolId lastSigByPoolId
        r ?!: SigTooFrequent poolId
        State.modify (\a -> a { vctxLastSigByPoolId = lastSigByPoolId' })

      -- TODO: if new cborg version is released, validation of SigId should be
      -- moved to the decoder, right now the decoder only verifies that we
      -- received the right amount of bytes.
      validateSigId sig ?! InvalidSigId

      --
      -- verify KES period
      --

      vctxPraosMaxKESEvo <$> State.get >>= \maxKESEvo -> do
        let endKESPeriod :: KESPeriod
            endKESPeriod = KESPeriod $ unKESPeriod startKESPeriod
                                     + fromIntegral maxKESEvo
        sigKESPeriod < endKESPeriod    ?! KESAfterEndOCERT endKESPeriod sigKESPeriod
        sigKESPeriod >= startKESPeriod ?! KESBeforeStartOCERT startKESPeriod sigKESPeriod

      --
      -- verify that the pool is registered and eligible to mint blocks
      --

      (vctxStakeMap <$> State.get) >>= \stakeMap ->
        case Map.lookup poolId stakeMap of
          Nothing ->
            vctxReadiness <$> State.get >>= \case
              Ready    -> left UnrecognizedPool
              NotReady -> left NotInitialized

          Just NotZeroSetSnapshot -> return ()

          -- pool unregistered and is ineligible to mint signatures
          Just ZeroSetSnapshot -> left PoolNotEligible

      --
      -- verify that our observations of ocertN are strictly monotonic
      --

      vctxOcertMap <$> State.get >>= \ocertMap ->
        case Map.alterF (\a -> (a, Just ocertN))
                        poolId
                        ocertMap of
          (Nothing, ocertMap')
            -- there is no ocert in the map, e.g. we're validating a signature
            -- produced by that SPO for the first time
            -> State.modify (\a -> a { vctxOcertMap = ocertMap' })
          (Just prevOcertN, ocertMap')
            -- QUESTION: should we be more strict with `<`!
            | prevOcertN <= ocertN -- `ocertN` is valid
            -> State.modify (\a -> a { vctxOcertMap = ocertMap' })

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
        poolId :: PoolId
        poolId = Ledger.hashKey (Ledger.VKey coldKey)

        startKESPeriod :: KESPeriod
        startKESPeriod = ocertKESPeriod

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
