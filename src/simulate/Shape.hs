module Simulate.Shape where

import Data.Ecstasy
import Linear.V2

import Types

type Position = V2 Float
type Size = V2 Float
data Shape = Rectangle Position Size

fromEntity :: (Monad world) => GameQueryT world Shape
fromEntity = do
    pos <- get position
    geo <- get geometry
    pure $ fromEntity' pos geo
  where
    fromEntity' :: (V2 Float) -> Geometry -> Shape
    fromEntity' (V2 x y) (Box w h) = Rectangle (V2 x y) (V2 w h)

x :: Shape -> Float
x (Rectangle (V2 x _) _) = x

y :: Shape -> Float
y (Rectangle (V2 _ y) _) = y

w :: Shape -> Float
w (Rectangle _ (V2 w _)) = w

h :: Shape -> Float
h (Rectangle _ (V2 _ h)) = h

left :: Shape -> Float
left (Rectangle (V2 x _) (V2 w _)) = x - (w / 2)

right :: Shape -> Float
right (Rectangle (V2 x _) (V2 w _)) = x + (w / 2)

top :: Shape -> Float
top (Rectangle (V2 _ y) (V2 _ h)) = y + (h / 2)

bottom :: Shape -> Float
bottom (Rectangle (V2 _ y) (V2 _ h)) = y - (h / 2)

-- | Returns a penetration vector if the shapes are overlapping.
-- | Otherwise it returns nothing.
-- |
-- | The penetration vector is a vector that can be applied to
-- | shape 1 to ensure both shapes are no longer overlapping.
penetration :: Shape -> Shape -> Maybe (V2 Float)
penetration a b | overlapping a b =
    Just $ snd $ bottomV `minOf` topV `minOf` leftV `minOf` rightV
  where
    bottomDistance = abs $ bottom a - top b
    topDistance    = abs $ top a - bottom a
    leftDistance   = abs $ left a - right b
    rightDistance  = abs $ right a - left b

    bottomVector = V2 0 bottomDistance
    topVector    = V2 0 (-topDistance)
    leftVector   = V2 (-leftDistance) 0
    rightVector  = V2 rightDistance 0

    bottomV = (bottomDistance, bottomVector)
    topV    = (topDistance, topVector)
    leftV   = (leftDistance, leftVector)
    rightV  = (rightDistance, rightVector)

    minOf :: (Float, V2 Float) -> (Float, V2 Float) -> (Float, V2 Float)
    minOf (d1, v1) (d2, v2) = if d1 < d2
                              then (d1, v1)
                              else (d2, v2)
penetration _ _ = Nothing

-- | Returns true if both shapes overlap
overlapping :: Shape -> Shape -> Bool
overlapping a b = overlappingX a b && overlappingY a b

-- | Returns true if both shapes overlap on the X axis
overlappingX :: Shape -> Shape -> Bool
overlappingX a b = left a < right b && right a > left b

-- | Returns true if both shapes overlap on the Y axis
overlappingY :: Shape -> Shape -> Bool
overlappingY a b = top a > bottom b && bottom a < top b
