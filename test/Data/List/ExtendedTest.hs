module Data.List.ExtendedTest where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

import Data.List.Extended

unit_pairs_smoketest :: IO ()
unit_pairs_smoketest = assertEqual "for (pairs [1,2,3])" (pairs [1, 2, 3]) [(1,2), (1,3), (2,3)]

-- | `pairs` gives the combinations of all elements of a list. Therefore the number of pairs
-- | returned should alwhays be equivalent to the number of possible binary combinations for
-- | a set of length n.
hprop_pairs_matches_binary_combinations :: Property
hprop_pairs_matches_binary_combinations = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    let expectedCombinations = choose (length xs) 2
    length (pairs xs) === expectedCombinations
  where
    choose :: Int -> Int -> Int
    choose _ 0 = 1
    choose 0 _ = 0
    choose n k = choose (n-1) (k-1) * n `div` k

hprop_ifNonEmptyElse_always_return_first_list_if_non_empty :: Property
hprop_ifNonEmptyElse_always_return_first_list_if_non_empty = property $ do
  nonEmpty <- forAll $ Gen.list (Range.linear 1 100) Gen.alpha
  maybeEmpty <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  let result = ifNonEmptyElse nonEmpty maybeEmpty
  result === nonEmpty

-- | We expect `ifNonEmptyElse` to always return one of it's
-- | input lists.
hprop_ifNonEmptyElse_always_return_either_list :: Property
hprop_ifNonEmptyElse_always_return_either_list = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  ys <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  let result = ifNonEmptyElse xs ys
  Hedgehog.assert (result == xs || result == ys)
