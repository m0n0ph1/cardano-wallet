{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Numeric.UtilSpec
    ( spec
    ) where

import Prelude

import Cardano.Numeric.Util
    ( equipartitionNatural
    , padCoalesce
    , partitionNatural
    , partitionNaturalWithPriority
    , zeroSmallestUntilSumMinimalDistanceToTarget
    )
import Data.List
    ( isSuffixOf )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes, isJust, isNothing )
import Data.Monoid
    ( Sum (..) )
import Data.Ratio
    ( (%) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , arbitrarySizedNatural
    , checkCoverage
    , choose
    , cover
    , forAll
    , oneof
    , property
    , shrink
    , shrinkIntegral
    , withMaxSuccess
    , (.&&.)
    , (.||.)
    , (===)
    , (==>)
    )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do

    describe "padCoalesce" $ do

        it "prop_padCoalesce_length" $
            property $ prop_padCoalesce_length @(Sum Int)
        it "prop_padCoalesce_sort" $
            property $ prop_padCoalesce_sort @(Sum Int)
        it "prop_padCoalesce_sum" $
            property $ prop_padCoalesce_sum @(Sum Int)

    describe "equipartitionNatural" $ do

        it "prop_equipartitionNatural_fair" $
            property prop_equipartitionNatural_fair
        it "prop_equipartitionNatural_length" $
            property prop_equipartitionNatural_length
        it "prop_equipartitionNatural_order" $
            property prop_equipartitionNatural_order
        it "prop_equipartitionNatural_sum" $
            property prop_equipartitionNatural_sum

    describe "partitionNatural" $ do

        it "prop_partitionNatural_length" $
            property prop_partitionNatural_length
        it "prop_partitionNatural_sum" $
            property prop_partitionNatural_sum
        it "prop_partitionNatural_fair" $
            withMaxSuccess 1000 $ checkCoverage prop_partitionNatural_fair

    describe "partitionNaturalWithPriority" $ do

        it "prop_partitionNaturalWithPriority_length" $
            property prop_partitionNaturalWithPriority_length
        it "prop_partitionNaturalWithPriority_sum" $
            property prop_partitionNaturalWithPriority_sum
        it "prop_partitionNaturalWithPriority_target_GT_sumWeights" $
            property prop_partitionNaturalWithPriority_target_GT_sumWeights
        it "prop_partitionNaturalWithPriority_target_EQ_sumWeights" $
            property prop_partitionNaturalWithPriority_target_EQ_sumWeights
        it "prop_partitionNaturalWithPriority_target_EQ_largestWeight" $
            property prop_partitionNaturalWithPriority_target_EQ_largestWeight
        it "prop_partitionNaturalWithPriority_target_LT_largestWeight" $
            property prop_partitionNaturalWithPriority_target_LT_largestWeight
        it "prop_partitionNaturalWithPriority_target_EQ_zero" $
            property prop_partitionNaturalWithPriority_target_EQ_zero

    describe "zeroSmallestUntilSumMinimalDistanceToTarget " $ do

        it "prop_zeroSmallestUntilSumMinimalDistanceToTarget_coverage" $
            property prop_zeroSmallestUntilSumMinimalDistanceToTarget_coverage
        it "prop_zeroSmallestUntilSumMinimalDistanceToTarget_equality" $
            property prop_zeroSmallestUntilSumMinimalDistanceToTarget_equality
        it "prop_zeroSmallestUntilSumMinimalDistanceToTarget_length" $
            property prop_zeroSmallestUntilSumMinimalDistanceToTarget_length
        it "prop_zeroSmallestUntilSumMinimalDistanceToTarget_suffix" $
            property prop_zeroSmallestUntilSumMinimalDistanceToTarget_suffix

--------------------------------------------------------------------------------
-- Coalescing values
--------------------------------------------------------------------------------

prop_padCoalesce_length
    :: (Monoid a, Ord a, Show a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_length source target =
    NE.length (padCoalesce source target) === NE.length target

prop_padCoalesce_sort
    :: (Monoid a, Ord a, Show a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_sort source target =
    NE.sort result === result
  where
    result = padCoalesce source target

prop_padCoalesce_sum
    :: (Monoid a, Ord a, Show a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_sum source target =
    F.fold source === F.fold (padCoalesce source target)

--------------------------------------------------------------------------------
-- Equipartitioning natural numbers
--------------------------------------------------------------------------------

-- Test that natural numbers are equipartitioned fairly:
--
-- Each portion must be within unity of the ideal portion.
--
prop_equipartitionNatural_fair
    :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_fair n count = (.||.)
    (difference === 0)
    (difference === 1)
  where
    difference :: Natural
    difference = F.maximum results - F.minimum results

    results :: NonEmpty Natural
    results = equipartitionNatural n count

prop_equipartitionNatural_length :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_length n count =
    NE.length (equipartitionNatural n count) === NE.length count

prop_equipartitionNatural_order :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_order n count =
    NE.sort results === results
  where
    results = equipartitionNatural n count

prop_equipartitionNatural_sum :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_sum n count =
    F.sum (equipartitionNatural n count) === n

--------------------------------------------------------------------------------
-- Partitioning natural numbers
--------------------------------------------------------------------------------

prop_partitionNatural_length
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_partitionNatural_length target weights =
    case partitionNatural target weights of
        Nothing -> F.sum weights === 0
        Just ps -> F.length ps === F.length weights

prop_partitionNatural_sum
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_partitionNatural_sum target weights =
    case partitionNatural target weights of
        Nothing -> F.sum weights === 0
        Just ps -> F.sum ps === target

-- | Check that portions are all within unity of ideal unrounded portions.
--
prop_partitionNatural_fair
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_partitionNatural_fair target weights =
    case partitionNatural target weights of
        Nothing -> F.sum weights === 0
        Just ps -> prop ps
  where
    prop portions = (.&&.)
        (F.all (uncurry (<=)) (NE.zip portions portionUpperBounds))
        (F.all (uncurry (>=)) (NE.zip portions portionLowerBounds))
      where
        portionUpperBounds = ceiling . computeIdealPortion <$> weights
        portionLowerBounds = floor   . computeIdealPortion <$> weights

        computeIdealPortion :: Natural -> Rational
        computeIdealPortion c
            = fromIntegral target
            * fromIntegral c
            % fromIntegral totalWeight

        totalWeight :: Natural
        totalWeight = F.sum weights

--------------------------------------------------------------------------------
-- Partitioning natural numbers with priority
--------------------------------------------------------------------------------

prop_partitionNaturalWithPriority_length
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_partitionNaturalWithPriority_length target weights =
    checkCoverage $
    cover 8 (isJust result)
        "isJust" $
    cover 1 (isNothing result)
        "isNothing " $
    case result of
        Nothing -> F.sum weights === 0
        Just ps -> F.length ps === F.length weights
  where
    result = partitionNaturalWithPriority target weights

prop_partitionNaturalWithPriority_sum
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_partitionNaturalWithPriority_sum target weights =
    checkCoverage $
    cover 8 (isJust result)
        "isJust" $
    cover 1 (isNothing result)
        "isNothing " $
    case result of
        Nothing -> F.sum weights === 0
        Just ps -> F.sum ps === target
  where
    result = partitionNaturalWithPriority target weights

prop_partitionNaturalWithPriority_target_GT_sumWeights
    :: NonEmpty Natural
    -> Property
prop_partitionNaturalWithPriority_target_GT_sumWeights weights =
    sumWeights > 0 ==>
    forAll genTarget $ \target ->
        (===)
            (partitionNaturalWithPriority target weights)
            (partitionNatural             target weights)
  where
    genTarget :: Gen Natural
    genTarget = fromIntegral @Integer @Natural <$> oneof
        [ pure (fromIntegral sumWeights)
        , choose
            ( fromIntegral sumWeights + 1
            , fromIntegral sumWeights * 1000
            )
        ]
    sumWeights :: Natural
    sumWeights = sum weights

prop_partitionNaturalWithPriority_target_EQ_sumWeights
    :: NonEmpty Natural
    -> Property
prop_partitionNaturalWithPriority_target_EQ_sumWeights weights =
    sumWeights > 0 ==>
        (===)
            (partitionNaturalWithPriority sumWeights weights)
            (partitionNatural             sumWeights weights)
  where
    sumWeights :: Natural
    sumWeights = sum weights

prop_partitionNaturalWithPriority_target_EQ_largestWeight
    :: NonEmpty Natural
    -> Property
prop_partitionNaturalWithPriority_target_EQ_largestWeight weights =
    sumWeights > 0 ==>
    NE.filter (> 0) result === [largestWeight]
  where
    Just result = partitionNaturalWithPriority largestWeight weights

    largestWeight :: Natural
    largestWeight = maximum weights

    sumWeights :: Natural
    sumWeights = sum weights

prop_partitionNaturalWithPriority_target_LT_largestWeight
    :: NonEmpty Natural
    -> Property
prop_partitionNaturalWithPriority_target_LT_largestWeight weights =
    sumWeights > 0 ==>
    forAll genTarget $ \target ->
        let Just result = partitionNaturalWithPriority target weights in
        NE.filter (> 0) result === [target]
  where
    genTarget :: Gen Natural
    genTarget = fromIntegral @Integer @Natural <$>
        choose (1, max 1 (fromIntegral largestWeight - 1))

    largestWeight :: Natural
    largestWeight = maximum weights

    sumWeights :: Natural
    sumWeights = sum weights

prop_partitionNaturalWithPriority_target_EQ_zero
    :: NonEmpty Natural
    -> Property
prop_partitionNaturalWithPriority_target_EQ_zero weights =
    sumWeights > 0 ==>
        result === (0 <$ weights)
  where
    Just result = partitionNaturalWithPriority 0 weights

    sumWeights :: Natural
    sumWeights = sum weights

--------------------------------------------------------------------------------
-- Minimizing the distance between a sum of weights and a target value.
--------------------------------------------------------------------------------

-- TODO: Use a dedicated data type to get the coverage we want.
--
-- It should test with
--
-- - different lengths of lists.
-- - arrange that we cover:
--     - one of the items being zeroed out
--     - all of the items being zeroed out
--     - other proportions
--
prop_zeroSmallestUntilSumMinimalDistanceToTarget_coverage
    :: NonEmpty Natural
    -> Natural
    -> Property
prop_zeroSmallestUntilSumMinimalDistanceToTarget_coverage as target =
    property $
    checkCoverage $
    cover 10 (asSum > target)
        "asSum > target" $
    cover 1 (asSum == target)
        "asSum = target" $
    cover 1 (asSum < target)
        "asSum < target" $
    cover 10 (rsSum > 0)
        "rsSum > 0" $
    cover 1 (rsNonZeroCount > 1)
        "rsNonZeroCount > 1" $
    True
  where
    asSum = F.sum as
    rsSum = F.sum rs
    rsNonZeroCount = length $ filter (> 0) $ F.toList rs
    rs = zeroSmallestUntilSumMinimalDistanceToTarget as target

prop_zeroSmallestUntilSumMinimalDistanceToTarget_equality
    :: NonEmpty Natural
    -> Natural
    -> Property
prop_zeroSmallestUntilSumMinimalDistanceToTarget_equality as target
    | total <= target =
        as === rs
    | otherwise =
        property $ F.all (\(r, a) -> r == a || r == 0) (rs `NE.zip` as)
  where
    rs = zeroSmallestUntilSumMinimalDistanceToTarget as target
    total = F.sum as

prop_zeroSmallestUntilSumMinimalDistanceToTarget_length
    :: NonEmpty Natural
    -> Natural
    -> Property
prop_zeroSmallestUntilSumMinimalDistanceToTarget_length as target =
    NE.length as ===
    NE.length (zeroSmallestUntilSumMinimalDistanceToTarget as target)

prop_zeroSmallestUntilSumMinimalDistanceToTarget_suffix
    :: NonEmpty Natural
    -> Natural
    -> Property
prop_zeroSmallestUntilSumMinimalDistanceToTarget_suffix as target =
    property $ dropWhile (== 0) rsSorted `isSuffixOf` asSorted
  where
    asSorted = NE.toList $ NE.sort as
    rsSorted = NE.toList $ NE.sort $
        zeroSmallestUntilSumMinimalDistanceToTarget as target

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink xs = catMaybes $ NE.nonEmpty <$> shrink (NE.toList xs)

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral
