module Simulate.Shape where

import Control.Monad
import Data.Ecstasy
import Linear.Metric
import Linear.V2

import Types
import Extra.Ord

type Position = V2 Float
type Size = V2 Float
data Shape = AABB Position Size
  deriving Show

fromEntity :: (Monad world) => GameQueryT world Shape
fromEntity = do
    pos <- get Types.position
    geo <- get geometry
    pure $ fromEntity' pos geo
  where
    fromEntity' :: (V2 Float) -> Geometry -> Shape
    fromEntity' (V2 x y) (Box w h) = AABB (V2 x y) (V2 w h)

center :: Shape -> Position
center (AABB pos _) = pos

size :: Shape -> Size
size (AABB _ size) = size

extents :: Shape -> Size
extents a = size a / 2

x :: Shape -> Float
x (AABB (V2 x _) _) = x

y :: Shape -> Float
y (AABB (V2 _ y) _) = y

w :: Shape -> Float
w (AABB _ (V2 w _)) = w

h :: Shape -> Float
h (AABB _ (V2 _ h)) = h

left :: Shape -> Float
left (AABB (V2 x _) (V2 w _)) = x - (w / 2)

right :: Shape -> Float
right (AABB (V2 x _) (V2 w _)) = x + (w / 2)

top :: Shape -> Float
top (AABB (V2 _ y) (V2 _ h)) = y + (h / 2)

bottom :: Shape -> Float
bottom (AABB (V2 _ y) (V2 _ h)) = y - (h / 2)

-- | Calculate the smallest point within the Shape
minPoint :: Shape -> V2 Float
minPoint a = center a - extents a

-- | Calculate the largest point within the Shape
maxPoint :: Shape -> V2 Float
maxPoint a = center a + extents a

-- | Returns true if the Shape contains the point
containsPoint :: V2 Float -> Shape -> Bool
containsPoint (V2 x y) a = (x > minX && x < maxX) && (y > minY && y < maxY)
  where (V2 minX minY) = minPoint a
        (V2 maxX maxY) = maxPoint a

-- | Returns true if the Shape contains the origin
containsOrigin :: Shape -> Bool
containsOrigin = containsPoint (V2 0 0)

-- | Calculate the minkowski difference of two axis-aligned bounding boxes
-- | The result is itself a Shape
-- |
-- | Useful properties of the minkowski difference:
-- | - If it encompasses the origin then the two input Shape's are colliding
-- | - The minimum distance between the origin and the minkowski difference is the
-- |   the distance between the two shapes
minkowskiDifference :: Shape -> Shape -> Shape
minkowskiDifference a b = AABB mdPos mdSize
  where mdTopLeft = minPoint a - maxPoint b
        mdSize = size a + size b
        mdPos = mdTopLeft + mdSize / 2

-- | Clamps a point such that it must be inside the Shape
clampPointToShape :: V2 Float -> Shape -> V2 Float
clampPointToShape (V2 x y) a = V2 cx cy
  where cx = clamp (left a) (right a) x
        cy = clamp (bottom a) (top a) y

-- | Find the closest point on the bounds of the Shape to the given point.
-- |
-- | If the point is outside the shape then clamping it will give us the closest point
-- |
-- | Otherwise we need to find the smallest distance between the point and
-- | each edge of the shape and then move the point by it.
closestPointOnBoundsToPoint :: V2 Float -> Shape -> V2 Float
closestPointOnBoundsToPoint p a = closest
  where pointInShape @ (V2 px py) = clampPointToShape p a
        leftVect =   V2 (left a) py
        rightVect =  V2 (right a) py
        topVect =    V2 px (top a)
        bottomVect = V2 px (bottom a)
        minV = minBy (distance pointInShape)
        closest = leftVect `minV` rightVect `minV` topVect `minV` bottomVect

-- | Returns a penetration vector if the shapes are overlapping.
-- | Otherwise it returns nothing.
-- |
-- | The penetration vector is a vector that can be applied to
-- | shape 1 to ensure both shapes are no longer overlapping.
penetration :: Shape -> Shape -> Maybe (V2 Float)
penetration a b | overlapping a b = Just penetrationVector
  where penetrationVector = closestPointOnBoundsToPoint (V2 0 0) (minkowskiDifference a b)
penetration _ _ = Nothing

-- | Returns true if both shapes overlap
overlapping :: Shape -> Shape -> Bool
overlapping a b = containsOrigin $ minkowskiDifference a b
