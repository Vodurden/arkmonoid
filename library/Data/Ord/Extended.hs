module Data.Ord.Extended
  ( module Data.Ord
  , clamp
  , minBy
  , maxBy
  ) where

import Data.Ord

-- | Returns n if it is between low and high.
-- | If it's lower then return low. Otherwise return high
clamp :: (Ord a)
        => a -- ^ The minimum number to return
        -> a -- ^ The maximum number to return
        -> a -- ^ The number to clamp
        -> a
clamp low high n = max low (min high n)

minBy :: (Ord b) => (a -> b) -> a -> a -> a
minBy f x y = if f x < f y
              then x
              else y

maxBy :: (Ord b) => (a -> b) -> a -> a -> a
maxBy f x y = if f x > f y
              then x
              else y
