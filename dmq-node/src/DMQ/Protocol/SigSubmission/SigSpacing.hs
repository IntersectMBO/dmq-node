-- | Limit how often a pool can issue signatures.
--
-- `sigExpiresAt` of any two signatures issued by the same pool must be at
-- least `DMQ.Policy.minSigDelay` apart.
--
-- The KES signature covers `sigExpiresAt`, so every node sees the same expiry.
-- A burst from another node's mempool passes the spacing check in any arrival
-- order if its expiries are far enough apart from each other and from those
-- already recorded for the same pool.  Other checks, such as the OCert counter
-- check, can depend on arrival order.
--
-- `sigExpiresAt` is chosen by the signer, but it must lie in
-- @[now, now + maxSigExpiresAtDelay)@ (see `DMQ.Policy.maxSigExpiresAtDelay`).
-- Thus a pool can have at most @maxSigExpiresAtDelay / minSigDelay@
-- signatures which have not expired yet, and over any period of time @D@ it
-- can issue at most about @(D + maxSigExpiresAtDelay) / minSigDelay@
-- signatures.  For this to hold, the value of an expired signature must still
-- be taken into account for @minSigDelay@ after it expired, otherwise a pool
-- could issue signatures with a short time to live at an arbitrary rate.
--
-- The module should be imported qualified.
--
module DMQ.Protocol.SigSubmission.SigSpacing
  ( SigSpacing
  , empty
  , insert
  , toList
  ) where

import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as OrdPSQ
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (DiffTime)
import Data.Time.Clock.POSIX (POSIXTime)


-- | `sigExpiresAt` of recently accepted signatures, grouped by a key, e.g.
-- a `DMQ.Protocol.SigSubmission.Type.PoolId`.
--
-- Invariants:
--
-- * every set is non-empty;
-- * the priority of a key is the maximum of its set.
--
newtype SigSpacing poolId = SigSpacing (OrdPSQ poolId POSIXTime (Set POSIXTime))
  deriving Show


empty :: SigSpacing poolId
empty = SigSpacing OrdPSQ.empty


-- | Record `sigExpiresAt` of a signature issued by @poolId@, unless it is
-- closer than @minSigDelay@ to `sigExpiresAt` of another signature issued by
-- the same @poolId@.
--
-- Values which are not larger than @now - minSigDelay@ are forgotten: they
-- cannot conflict with a signature which has not expired.
--
insert :: Ord poolId
       => DiffTime
       -- ^ @minSigDelay@
       -> POSIXTime
       -- ^ current time
       -> poolId
       -> POSIXTime
       -- ^ `sigExpiresAt`
       -> SigSpacing poolId
       -> Either POSIXTime (SigSpacing poolId)
       -- ^ either `sigExpiresAt` of a conflicting signature or the updated
       -- `SigSpacing`
insert minSigDelay now poolId expiresAt (SigSpacing psq) =
    case Set.lookupGT (max horizon (expiresAt - delay)) values of
      Just conflict | conflict < expiresAt + delay -> Left conflict
      _ ->
        -- A rejection keeps the original queue, so prune other pools only
        -- on success.
        let values' = Set.insert expiresAt
                    . Set.dropWhileAntitone (<= horizon)
                    $ values
            psq'    = OrdPSQ.insert poolId (Set.findMax values') values'
                    . snd
                    . OrdPSQ.atMostView horizon
                    $ psq
        in Right (SigSpacing psq')
  where
    delay :: POSIXTime
    delay = realToFrac minSigDelay

    -- values which are not larger than `horizon` are forgotten
    horizon :: POSIXTime
    horizon = now - delay

    values :: Set POSIXTime
    values = maybe Set.empty snd (OrdPSQ.lookup poolId psq)


-- | All recorded values, in ascending order of keys and values.  Some values
-- not larger than @now - minSigDelay@ might still be present.
--
toList :: SigSpacing key -> [(key, [POSIXTime])]
toList (SigSpacing psq) =
  [ (key, Set.toAscList values)
  | (key, _, values) <- OrdPSQ.toAscList psq
  ]
