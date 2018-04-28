module Data.List.Extended
  ( module Data.List
  , pairs
  , ifNonEmptyElse
  ) where

import Data.List

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

-- | Returns the first non-empty list.
-- |
-- | Returns the empty list if both lists are
-- | empty.
ifNonEmptyElse :: [a] -> [a] -> [a]
ifNonEmptyElse [] ys = ys
ifNonEmptyElse xs _  = xs
