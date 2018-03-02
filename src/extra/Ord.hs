module Extra.Ord where

-- | Returns n if it is between low and high.
-- | If it's lower then return low. Otherwise return high
clamp :: (Ord a)
        => a -- ^ The minimum number to return
        -> a -- ^ The maximum number to return
        -> a -- ^ The number to clamp
        -> a
clamp low high n = max low (min high n)
