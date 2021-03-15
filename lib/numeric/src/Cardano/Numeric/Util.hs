{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Numeric.Util
    (
      -- * Coalescing values
      padCoalesce

      -- * Partitioning natural numbers
    , equipartitionNatural
    , partitionNatural
    , partitionNaturalWithPriority
    , unsafePartitionNatural

      -- * Miscellaneous
    , cumulativeSum
    , dropUntilSumMinimalDistanceToTarget
    , zeroSmallestUntilSumMinimalDistanceToTarget

      -- * Partial orders
    , inAscendingPartialOrder

    ) where

import Prelude hiding
    ( round )

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Control.Arrow
    ( (&&&) )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Ord
    ( Down (..), comparing )
import Data.Ratio
    ( (%) )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import Safe
    ( tailMay )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Extra as NE

--------------------------------------------------------------------------------
-- Public functions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Coalescing values
--------------------------------------------------------------------------------

-- | Adjusts the source list so that its length is the same as the target list,
--   either by padding the list, or by coalescing a subset of the elements,
--   while preserving the total sum.
--
-- If the source list is shorter than the target list, this function repeatedly
-- inserts 'mempty' into the list until the desired length has been reached.
--
-- If the source list is longer than the target list, this function repeatedly
-- coalesces the smallest pair of elements with '<>' until the desired length
-- has been reached.
--
-- The resulting list is guaranteed to be sorted into ascending order, and the
-- sum of the elements is guaranteed to be the same as the sum of elements in
-- the source list.
--
-- Examples (shown with ordinary list notation):
--
-- >>> padCoalesce [Sum 1] (replicate 4 ())
-- [Sum 0, Sum 0, Sum 0, Sum 1]
--
-- >>> padCoalesce [Sum (-1)] (replicate 4 ())
-- [Sum (-1), Sum 0, Sum 0, Sum 0]
--
-- >>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 3 ())
-- [Sum 3, Sum 4, Sum 8]
--
-- >>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 2 ())
-- [Sum 7, Sum 8]
--
-- >>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 1 ())
-- [Sum 15]
--
padCoalesce :: forall m a. (Monoid m, Ord m)
    => NonEmpty m
    -- ^ Source list
    -> NonEmpty a
    -- ^ Target list
    -> NonEmpty m
padCoalesce sourceUnsorted target
    | sourceLength < targetLength =
        applyN (targetLength - sourceLength) pad source
    | sourceLength > targetLength =
        applyN (sourceLength - targetLength) coalesce source
    | otherwise =
        source
  where
    source = NE.sort sourceUnsorted

    sourceLength = NE.length source
    targetLength = NE.length target

    pad :: NonEmpty m -> NonEmpty m
    pad = NE.insert mempty

    coalesce :: NonEmpty m -> NonEmpty m
    coalesce (x :| y : zs) = NE.insert (x <> y) zs
    coalesce xs = xs

--------------------------------------------------------------------------------
-- Partitioning natural numbers
--------------------------------------------------------------------------------

-- | Computes the equipartition of a natural number into 'n' smaller numbers.
--
-- An /equipartition/ of a natural number 'n' is a /partition/ of that number
-- into 'n' smaller numbers whose values differ by no more than 1.
--
-- The resultant list is sorted in ascending order.
--
equipartitionNatural
    :: HasCallStack
    => Natural
    -- ^ The natural number to be partitioned.
    -> NonEmpty a
    -- ^ Represents the number of portions in which to partition the number.
    -> NonEmpty Natural
    -- ^ The partitioned numbers.
equipartitionNatural n count =
    -- Note: due to the behaviour of the underlying partition algorithm, a
    -- simple list reversal is enough to ensure that the resultant list is
    -- sorted in ascending order.
    NE.reverse $ unsafePartitionNatural n (1 <$ count)

-- | Partitions a natural number into a number of parts, where the size of each
--   part is proportional to the size of its corresponding element in the given
--   list of weights, and the number of parts is equal to the number of weights.
--
-- Examples:
--
--      >>> partitionNatural 9 (1 :| [1, 1])
--      Just (3 :| [3, 3])
--
--      >>> partitionNatural 10 (1 :| [])
--      10
--
--      >>> partitionNatural 30 (1 :| [2, 4, 8])
--      Just (2 :| [4, 8, 16])
--
-- Pre-condition: there must be at least one non-zero weight.
--
-- If the pre-condition is not satisfied, this function returns 'Nothing'.
--
-- If the pre-condition is satisfied, this function guarantees that:
--
--  1.  The length of the resulting list is identical to the length of the
--      specified list:
--
--      >>> fmap length (partitionNatural n weights) == Just (length weights)
--
--  2.  The sum of elements in the resulting list is equal to the original
--      natural number:
--
--      >>> fmap sum (partitionNatural n weights) == Just n
--
--  3.  The size of each element in the resulting list is within unity of the
--      ideal proportion.
--
partitionNatural
    :: Natural
        -- ^ Natural number to partition
    -> NonEmpty Natural
        -- ^ List of weights
    -> Maybe (NonEmpty Natural)
partitionNatural target weights
    | totalWeight == 0 = Nothing
    | otherwise = Just portionsRounded
  where
    portionsRounded :: NonEmpty Natural
    portionsRounded
        -- 1. Start with the list of unrounded portions:
        = portionsUnrounded
        -- 2. Attach an index to each portion, so that we can remember the
        --    original order:
        & NE.zip indices
        -- 3. Sort the portions into descending order of their fractional
        --    parts, and then sort each subsequence with equal fractional
        --    parts into descending order of their integral parts:
        & NE.sortBy (comparing (Down . (fractionalPart &&& integralPart) . snd))
        -- 4. Apply pre-computed roundings to each portion:
        & NE.zipWith (fmap . round) roundings
        -- 5. Restore the original order:
        & NE.sortBy (comparing fst)
        -- 6. Strip away the indices:
        & fmap snd
      where
        indices :: NonEmpty Int
        indices = 0 :| [1 ..]

    portionsUnrounded :: NonEmpty Rational
    portionsUnrounded = computeIdealPortion <$> weights
      where
        computeIdealPortion c
            = fromIntegral target
            * fromIntegral c
            % fromIntegral totalWeight

    roundings :: NonEmpty RoundingDirection
    roundings =
        applyN shortfall (NE.cons RoundUp) (NE.repeat RoundDown)
      where
        shortfall
            = fromIntegral target
            - fromIntegral @Integer
                (F.sum $ round RoundDown <$> portionsUnrounded)

    totalWeight :: Natural
    totalWeight = F.sum weights

-- Properties:
--
-- >>> sum    (partitionNaturalWithPriority n xs) == n
-- >>> length (partitionNaturalWithPriority n xs) == length xs
--
-- Case #1:
--
-- The target value is greater than or equal to the sum of weights.
--
-- In this case, proportionality is maintained with all weights:
--
-- >>> partitionNaturalWithPriority 200 [20, 30, 50, 100] == [20,  30,  50, 100]
-- >>> partitionNaturalWithPriority 400 [20, 30, 50, 100] == [40,  60, 100, 200]
-- >>> partitionNaturalWithPriority 800 [20, 30, 50, 100] == [80, 120, 200, 200]
--
-- Case #2:
--
-- The target value is less than or equal to the sum of weights, but greater
-- than or equal to the largest weight.
--
-- In this case, as the target weight decreases, the approximate magnitudes of
-- the largest weights are maintained, but the smallest weights are sacrificed:
--
-- >>> partitionNaturalWithPriority 200 [20, 30, 50, 100] == [20, 30, 50, 100]
-- >>> partitionNaturalWithPriority 199 [20, 30, 50, 100] == [20, 30, 50,  99]
-- >>> ...
-- >>> partitionNaturalWithPriority 191 [20, 30, 50, 100] == [19, 29, 48,  95]
-- >>> partitionNaturalWithPriority 190 [20, 30, 50, 100] == [19, 28, 48,  95]
-- >>> partitionNaturalWithPriority 189 [20, 30, 50, 100] == [ 0, 31, 53, 105]
-- >>> partitionNaturalWithPriority 188 [20, 30, 50, 100] == [ 0, 31, 52, 105]
-- >>> ...
-- >>> partitionNaturalWithPriority 166 [20, 30, 50, 100] == [ 0, 28, 46,  92]
-- >>> partitionNaturalWithPriority 165 [20, 30, 50, 100] == [ 0, 27, 46,  92]
-- >>> partitionNaturalWithPriority 164 [20, 30, 50, 100] == [ 0,  0, 55, 109]
-- >>> partitionNaturalWithPriority 163 [20, 30, 50, 100] == [ 0,  0, 54, 109]
-- >>> ...
-- >>> partitionNaturalWithPriority 126 [20, 30, 50, 100] == [ 0,  0, 42,  84]
-- >>> partitionNaturalWithPriority 125 [20, 30, 50, 100] == [ 0,  0, 42,  83]
-- >>> partitionNaturalWithPriority 124 [20, 30, 50, 100] == [ 0,  0,  0, 124]
-- >>> partitionNaturalWithPriority 123 [20, 30, 50, 100] == [ 0,  0,  0, 123]
-- >>> ...
-- >>> partitionNaturalWithPriority 101 [20, 30, 50, 100] == [ 0,  0,  0, 101]
-- >>> partitionNaturalWithPriority 100 [20, 30, 50, 100] == [ 0,  0,  0, 100]
--
-- Case #3:
--
-- The target value is less than or equal to the largest weight.
--
-- In this case, all value is assigned to the largest weight:
--
-- >>> partitionNaturalWithPriority 100 [20, 30, 50, 100] == [0, 0, 0, 100]
-- >>> partitionNaturalWithPriority  80 [20, 30, 50, 100] == [0, 0, 0,  80]
-- >>> partitionNaturalWithPriority  60 [20, 30, 50, 100] == [0, 0, 0,  60]
-- >>> partitionNaturalWithPriority  40 [20, 30, 50, 100] == [0, 0, 0,  40]
-- >>> partitionNaturalWithPriority  20 [20, 30, 50, 100] == [0, 0, 0,  20]
-- >>> partitionNaturalWithPriority   0 [20, 30, 50, 100] == [0, 0, 0,   0]
--
partitionNaturalWithPriority
    :: Natural
        -- ^ Natural number to partition.
    -> NonEmpty Natural
        -- ^ List of weights.
    -> Maybe (NonEmpty Natural)
        -- ^ The partitioned result.
partitionNaturalWithPriority target weights
    = partitionNatural target
    $ zeroSmallestUntilSumMinimalDistanceToTarget weights target

dropUntilSumMinimalDistanceToTarget
    :: forall a. (Num a, Ord a)
    => NonEmpty a
    -- ^ List from which to drop values.
    -> a
    -- ^ Target sum value.
    -> NonEmpty a
    -- ^ Suffix of original list.
dropUntilSumMinimalDistanceToTarget as target =
    NE.zipReversedWith
        (curry fst)
        (as)
        (NE.dropWhileRetainLast (> minimumDistance) distances)
  where
    distances :: NonEmpty a
    distances =
        NE.zipWith
            (distance)
            (NE.withReversed cumulativeSum as)
            (NE.repeat target)

    minimumDistance :: a
    minimumDistance = F.minimum distances

zeroSmallestUntilSumMinimalDistanceToTarget
    :: (Num a, Ord a)
    => NonEmpty a
    -- ^ List with values to erase.
    -> a
    -- ^ Target sum value.
    -> NonEmpty a
    -- ^ Original list with one or more values erased.
zeroSmallestUntilSumMinimalDistanceToTarget as target =
    NE.withSorted eraseValues as
  where
    eraseValues
        = flip NE.padHeadWith (0 <$ as)
        . flip dropUntilSumMinimalDistanceToTarget target

--------------------------------------------------------------------------------
-- Unsafe partitioning
--------------------------------------------------------------------------------

-- | Partitions a natural number into a number of parts, where the size of each
--   part is proportional to the size of its corresponding element in the given
--   list of weights, and the number of parts is equal to the number of weights.
--
-- Throws a run-time error if the sum of weights is equal to zero.
--
unsafePartitionNatural
    :: HasCallStack
    => Natural
    -- ^ Natural number to partition
    -> NonEmpty Natural
    -- ^ List of weights
    -> NonEmpty Natural
unsafePartitionNatural target =
    fromMaybe zeroWeightSumError . partitionNatural target
  where
    zeroWeightSumError = error $ unwords
        [ "unsafePartitionNatural:"
        , "specified weights must have a non-zero sum."
        ]

--------------------------------------------------------------------------------
-- Partial orders
--------------------------------------------------------------------------------

inAscendingPartialOrder :: (Foldable f, PartialOrd a) => f a -> Bool
inAscendingPartialOrder = all (uncurry leq) . consecutivePairs . F.toList

--------------------------------------------------------------------------------
-- Internal types and functions
--------------------------------------------------------------------------------

-- Apply the same function multiple times to a value.
--
applyN :: Int -> (a -> a) -> a -> a
applyN n f = F.foldr (.) id (replicate n f)

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs xs = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys

cumulativeSum :: Num a => NonEmpty a -> NonEmpty a
cumulativeSum = NE.scanl1 (+)

distance :: (Num a, Ord a) => a -> a -> a
distance a b
    | a < b = b - a
    | a > b = a - b
    | otherwise = 0

-- Extract the fractional part of a rational number.
--
-- Examples:
--
-- >>> fractionalPart (3 % 2)
-- 1 % 2
--
-- >>> fractionalPart (11 % 10)
-- 1 % 10
--
fractionalPart :: Rational -> Rational
fractionalPart = snd . properFraction @_ @Integer

integralPart :: Rational -> Integer
integralPart = floor

-- | Indicates a rounding direction to be used when converting from a
--   fractional value to an integral value.
--
-- See 'round'.
--
data RoundingDirection
    = RoundUp
      -- ^ Round up to the nearest integral value.
    | RoundDown
      -- ^ Round down to the nearest integral value.
    deriving (Eq, Show)

-- | Use the given rounding direction to round the given fractional value,
--   producing an integral result.
--
round :: (RealFrac a, Integral b) => RoundingDirection -> a -> b
round = \case
    RoundUp -> ceiling
    RoundDown -> floor
