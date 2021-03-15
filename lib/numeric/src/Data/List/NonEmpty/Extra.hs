module Data.List.NonEmpty.Extra
    where

import Prelude

import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty (..) )

import qualified Data.List.NonEmpty as NE

dropWhileRetainLast
    :: (a -> Bool)
    -> NonEmpty a
    -> NonEmpty a
dropWhileRetainLast f as =
    case NE.dropWhile f as of
        [] ->
            NE.last as :| []
        (x : xs) ->
            x :| xs

padHeadWith
    :: NonEmpty a
    -- ^ Determines the suffix of the result.
    -> NonEmpty a
    -- ^ Determines the prefix and length of the result.
    -> NonEmpty a
padHeadWith suffix padding
    | Just prefix <- NE.nonEmpty mPrefix =
        prefix <> suffix
    | otherwise =
        suffix
  where
    mPrefix = NE.take (NE.length padding - NE.length suffix) padding

withReversed
    :: (NonEmpty a -> NonEmpty a)
    -- ^ A length-preserving transformation on a reversed list.
    -> (NonEmpty a -> NonEmpty a)
    -- ^ The transformed result with the original order restored.
withReversed f = NE.reverse . f . NE.reverse

withSorted
    :: Ord a
    => (NonEmpty a -> NonEmpty b)
    -- ^ A length-preserving transformation on a sorted list.
    -> (NonEmpty a -> NonEmpty b)
    -- ^ The transformed result with the original order restored.
withSorted = withSortedOn id

withSortedOn
    :: Ord o
    => (a -> o)
    -- ^ A function that maps an element to a sortable value.
    -> (NonEmpty a -> NonEmpty b)
    -- ^ A length-preserving transformation on a sorted list.
    -> (NonEmpty a -> NonEmpty b)
    -- ^ The transformed result with the original order restored.
withSortedOn order f values = valuesSorted
    & f
    & NE.zip indices
    & NE.sortWith fst
    & fmap snd
  where
    (indices, valuesSorted) = values
        & NE.zip (NE.iterate succ (0 :: Int))
        & NE.sortWith (order . snd)
        & NE.unzip

zipReversedWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipReversedWith f as bs
    = NE.reverse
    $ NE.zipWith f (NE.reverse as) (NE.reverse bs)
