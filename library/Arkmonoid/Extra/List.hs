module Arkmonoid.Extra.List where

import Data.List

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

-- | Returns the first non-e
ifNonEmptyElse :: [a] -> [a] -> [a]
ifNonEmptyElse [] ys = ys
ifNonEmptyElse xs _  = xs
