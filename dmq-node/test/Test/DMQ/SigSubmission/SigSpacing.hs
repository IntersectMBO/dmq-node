{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.DMQ.SigSubmission.SigSpacing (tests) where

import Control.Monad (foldM)

import Data.Either (isRight)
import Data.List (sort)
import Data.Time.Clock.POSIX (POSIXTime)

import DMQ.Protocol.SigSubmission.SigSpacing (SigSpacing)
import DMQ.Protocol.SigSubmission.SigSpacing qualified as SigSpacing

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "DMQ.Protocol.SigSubmission.SigSpacing"
  [ testProperty "model"     prop_sigSpacing_model
  , testProperty "any order" prop_sigSpacing_anyOrder
  ]


-- | `sigExpiresAt` of signatures must be smaller than @now + maxDelay@.
--
maxDelay :: Int
maxDelay = 1800

-- | An arbitrary starting time.
--
startTime :: POSIXTime
startTime = 1_000_000


-- | Number of values (per key) which are at least `minSigDelay` apart and
-- lie in an interval of length `maxDelay`.
--
maxValues :: Int -> Int
maxValues minSigDelay = ceiling (fromIntegral maxDelay / fromIntegral minSigDelay :: Double)


-- | Conflicts of a value with values accepted by a model.
--
modelConflicts :: POSIXTime
               -- ^ `minSigDelay`
               -> POSIXTime
               -- ^ current time
               -> [(Int, POSIXTime)]
               -- ^ accepted keys and values
               -> Int
               -- ^ key
               -> POSIXTime
               -- ^ value
               -> [POSIXTime]
modelConflicts delay now accepted key value =
  [ value'
  | (key', value') <- accepted
  , key' == key
  , value' > now - delay
  , abs (value - value') < delay
  ]


-- | An `SigSpacing.insert` operation: time elapsed since the previous operation
-- (in milliseconds), a key and `sigExpiresAt` offset since the current time
-- (in seconds).
--
data InsertOp = InsertOp {
    opElapsedMs :: Int,
    opKey       :: Int,
    opOffset    :: Int
  }
  deriving Show

instance Arbitrary InsertOp where
    arbitrary =
      InsertOp <$> frequency [ (3, chooseInt (0, 1_000))
                             , (2, chooseInt (0, 60_000))
                             , (1, chooseInt (0, 300_000))
                             ]
               <*> chooseInt (0, 3)
               <*> chooseInt (-120, maxDelay - 1)

    shrink op@InsertOp { opElapsedMs, opKey, opOffset } =
         [ op { opElapsedMs = a } | a <- shrink opElapsedMs, a >= 0 ]
      ++ [ op { opKey = a }       | a <- shrink opKey, a >= 0 ]
      ++ [ op { opOffset = a }    | a <- shrink opOffset, a >= -120 ]

-- | `minSigDelay` (in seconds) and a sequence of operations.
--
data InsertOps = InsertOps Int [InsertOp]
  deriving Show

instance Arbitrary InsertOps where
    arbitrary = InsertOps <$> chooseInt (1, 120) <*> arbitrary
    shrink (InsertOps minSigDelay ops) =
         [ InsertOps minSigDelay' ops | minSigDelay' <- shrink minSigDelay, minSigDelay' >= 1 ]
      ++ [ InsertOps minSigDelay ops' | ops' <- shrink ops ]


-- | `SigSpacing.insert` agrees with a simple model: a value is rejected iff the
-- same key has an accepted value, larger than @now - minSigDelay@, closer
-- than @minSigDelay@.  After each insert:
--
-- * values of a key are at least @minSigDelay@ apart;
-- * values larger than @now - minSigDelay@ are the same as in the model, i.e.
--   no value which can still conflict is forgotten;
-- * the number of values of a key which have not expired is bounded by
--   @maxDelay / minSigDelay@.
--
prop_sigSpacing_model :: InsertOps -> Property
prop_sigSpacing_model (InsertOps minSigDelay ops) =
    let (props, conflicts) = go startTime SigSpacing.empty [] ops
    in tabulate "conflicts" [bucket conflicts]
     $ conjoin props
  where
    delay :: POSIXTime
    delay = fromIntegral minSigDelay

    bucket :: Int -> String
    bucket 0 = "0"
    bucket n | n < 5     = "1-4"
             | otherwise = ">=5"

    go :: POSIXTime
       -> SigSpacing Int
       -> [(Int, POSIXTime)]
       -> [InsertOp]
       -> ([Property], Int)
    go _ _ _ [] = ([], 0)
    go now0 spacing accepted (op@InsertOp { opElapsedMs, opKey, opOffset } : ops') =
        case SigSpacing.insert (fromIntegral minSigDelay) now opKey value spacing of
          Left conflict ->
            let prop = counterexample (show op)
                     . counterexample ("unexpected conflict: " ++ show conflict)
                     $ conflict `elem` conflicts
                (props, n) = go now spacing accepted ops'
            in (prop : props, succ n)
          Right spacing' ->
            let accepted' = (opKey, value) : accepted
                prop = counterexample (show op)
                     $ counterexample ("missed conflicts: " ++ show conflicts)
                         (null conflicts)
                  .&&. invariants spacing' accepted'
                (props, n) = go now spacing' accepted' ops'
            in (prop : props, n)
      where
        now :: POSIXTime
        now = now0 + fromIntegral opElapsedMs / 1000

        value :: POSIXTime
        value = fromIntegral (floor now :: Integer) + fromIntegral opOffset

        conflicts = modelConflicts delay now accepted opKey value

        invariants :: SigSpacing Int -> [(Int, POSIXTime)] -> Property
        invariants spacing' accepted' =
               counterexample "live values differ from the model"
                 (filter isLive recorded === sort (filter isLive accepted'))
          .&&. conjoin
            [ counterexample ("key " ++ show key ++ ": " ++ show values) $
                   counterexample "values not spaced"
                     (and (zipWith (\a b -> b - a >= delay) values (drop 1 values)))
              .&&. counterexample "too many values which have not expired"
                     (length (filter (>= now) values) <= maxValues minSigDelay)
            | (key, values) <- valuesByKey
            ]
          where
            valuesByKey = SigSpacing.toList spacing'
            recorded = [ (key, v) | (key, values) <- valuesByKey, v <- values ]
            isLive (_, v) = v > now - delay


-- | Keys with values (`sigExpiresAt` offsets since `startTime`), which are at
-- least `minSigDelay` apart for each key, in an arbitrary order.
--
data SpacedValues = SpacedValues Int [(Int, Int)]
  deriving Show

instance Arbitrary SpacedValues where
    arbitrary = do
      minSigDelay <- chooseInt (1, 120)
      keys <- chooseInt (1, 4)
      values <- concat <$> traverse
        (\key -> do
          start <- chooseInt (0, minSigDelay - 1)
          gaps  <- listOf (chooseInt (minSigDelay, 3 * minSigDelay))
          return [ (key, offset)
                 | offset <- takeWhile (< maxDelay) (scanl (+) start gaps)
                 ])
        [1 .. keys]
      SpacedValues minSigDelay <$> shuffle values

    -- removing values preserves spacing
    shrink (SpacedValues minSigDelay values) =
      SpacedValues minSigDelay <$> shrinkList (const []) values


-- | Starting from `SigSpacing.empty`, values at least @minSigDelay@ apart for
-- each key pass the spacing check in any insertion order.
--
prop_sigSpacing_anyOrder :: SpacedValues -> Property
prop_sigSpacing_anyOrder (SpacedValues minSigDelay values) =
         counterexample "rejected" (isRight (insertAll values))
    .&&. counterexample "rejected in reverse order" (isRight (insertAll (reverse values)))
  where
    insertAll :: [(Int, Int)] -> Either POSIXTime (SigSpacing Int)
    insertAll =
      foldM (\spacing (key, offset) ->
              SigSpacing.insert (fromIntegral minSigDelay) startTime key
                                (startTime + fromIntegral offset) spacing)
            SigSpacing.empty
