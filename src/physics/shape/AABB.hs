module Physics.Shape.AABB where

import Physics.Shape.Types

import Linear.Metric
import Linear.V2

import Extra.Ord

center :: AABB -> Point
center aabb @ (AABB minP maxP) = maxP - (size aabb / 2)

size :: AABB -> Size
size (AABB minP maxP) = abs $ maxP - minP

left :: AABB -> Float
left shape = let (V2 x _) = minPoint shape in x

right :: AABB -> Float
right shape = let (V2 x _) = maxPoint shape in x

top :: AABB -> Float
top shape = let (V2 _ y) = maxPoint shape in y

bottom :: AABB -> Float
bottom shape = let (V2 _ y) = minPoint shape in y

topLeft :: AABB -> Point
topLeft shape = V2 (left shape) (top shape)

topRight :: AABB -> Point
topRight shape = V2 (right shape) (top shape)

bottomLeft :: AABB -> Point
bottomLeft shape = V2 (left shape) (bottom shape)

bottomRight :: AABB -> Point
bottomRight shape = V2 (right shape) (bottom shape)

-- | Returns a list of lines that make up this AABB
segments :: AABB -> [Segment]
segments a =
  [ Segment (topLeft a) (topRight a)
  , Segment (topRight a) (bottomRight a)
  , Segment (topLeft a) (bottomLeft a)
  , Segment (bottomLeft a) (bottomRight a)
  ]

-- | Calculate the smallest point within the AABB
minPoint :: AABB -> Point
minPoint (AABB min _) = min

-- | Calculate the largest point within the AABB
maxPoint :: AABB -> Point
maxPoint (AABB _ max) = max

-- | Returns true if the AABB contains the point
containsPoint :: AABB -> Point -> Bool
containsPoint a (V2 x y) = (x > minX && x < maxX) && (y > minY && y < maxY)
  where (V2 minX minY) = minPoint a
        (V2 maxX maxY) = maxPoint a

-- | Returns true if the aabb contains the origin
containsOrigin :: AABB -> Bool
containsOrigin shape = containsPoint shape (V2 0 0)

-- | Calculate the minkowski difference of two axis-aligned bounding boxes
-- | The result is itself a AABB
-- |
-- | Useful properties of the minkowski difference:
-- | - If it encompasses the origin then the two input AABB's are colliding
-- | - The minimum distance between the origin and the minkowski difference is the
-- |   the distance between the two shapes
minkowskiDifference :: AABB -> AABB -> AABB
minkowskiDifference a b = AABB mdMin mdMax
  where (V2 mdLeft mdBottom) = minPoint a - maxPoint b
        (V2 mdWidth mdHeight) = size a + size b
        mdMin = V2 mdLeft mdBottom
        mdMax = V2 (mdLeft + mdWidth) (mdBottom + mdHeight)

-- | Clamps a point such that it must be inside the AABB
clampPointToAABB :: V2 Float -> AABB -> V2 Float
clampPointToAABB (V2 x y) a = V2 cx cy
  where cx = clamp (left a) (right a) x
        cy = clamp (bottom a) (top a) y

-- | Find the closest point on the bounds of the AABB to the given point.
-- |
-- | If the point is outside the shape then clamping it will give us the closest point
-- |
-- | Otherwise we need to find the smallest distance between the point and
-- | each edge of the shape and then move the point by it.
closestPointOnBoundsToPoint :: V2 Float -> AABB -> V2 Float
closestPointOnBoundsToPoint p a = closest
  where pointInAABB @ (V2 px py) = clampPointToAABB p a
        leftVect =   V2 (left a) py
        rightVect =  V2 (right a) py
        topVect =    V2 px (top a)
        bottomVect = V2 px (bottom a)
        minV = minBy (distance pointInAABB)
        closest = leftVect `minV` rightVect `minV` topVect `minV` bottomVect

-- | Returns a penetration vector if the shapes are overlapping.
-- | Otherwise it returns nothing.
-- |
-- | The penetration vector is a vector that can be applied to
-- | shape 1 to ensure both shapes are no longer overlapping.
penetration :: AABB -> AABB -> Maybe (V2 Float)
penetration a b | overlapping a b = Just penetrationVector
  where penetrationVector = closestPointOnBoundsToPoint (V2 0 0) (minkowskiDifference a b)
penetration _ _ = Nothing

-- | Returns true if both shapes overlap
overlapping :: AABB -> AABB -> Bool
overlapping a b = containsOrigin $ minkowskiDifference a b
