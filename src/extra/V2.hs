module Extra.V2 where

import Linear.V2
import Linear.Matrix
import Linear.Metric

-- | Outputs a vector by reflecting it across another vector
-- |
-- | The resultant vector still passes through b
reflect :: V2 Float -> V2 Float -> V2 Float
reflect a b = a + offset
  where
    n = normalize b
    d = dot a n
    offset = negate $ 2 * V2 d d * n
