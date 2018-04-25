module Arkmonoid.Physics.Shape.AABB where

import Control.Lens
import Linear.Metric
import Linear.V2

import Arkmonoid.Extra.Ord
import Arkmonoid.Physics.Shape.Types

-- | Construct a AABB with a given width and height at a given point
mkAABB :: Point -> Size -> AABB
mkAABB p s =
  let extents = (s/2)
  in AABB (p - extents) (p + extents)

-- | Move the AABB by the given amount
moveBy :: V2 Float -> AABB -> AABB
moveBy amount = over minPoint (+amount) . over maxPoint (+amount)

-- | Moves the AABB to the given point
moveTo :: Point -> AABB -> AABB
moveTo p aabb =
  let movementAmount = p - center aabb
  in moveBy movementAmount aabb

center :: AABB -> Point
center aabb = aabb^.maxPoint - (size aabb / 2)

size :: AABB -> Size
size aabb = abs $ aabb^.maxPoint - aabb^.minPoint

left :: AABB -> Float
left shape = shape^.minPoint._x

right :: AABB -> Float
right shape = shape^.maxPoint._x

top :: AABB -> Float
top shape = shape^.maxPoint._y

bottom :: AABB -> Float
bottom shape = shape^.minPoint._y

topLeft :: AABB -> Point
topLeft shape = V2 (left shape) (top shape)

topRight :: AABB -> Point
topRight shape = shape^.maxPoint

bottomLeft :: AABB -> Point
bottomLeft shape = shape^.minPoint

bottomRight :: AABB -> Point
bottomRight shape = V2 (right shape) (bottom shape)

-- | Returns a list of lines that make up this AABB
segments :: AABB -> [Segment]
segments a =
  let tl = topLeft a
      tr = topRight a
      bl = bottomLeft a
      br = bottomRight a
  in [ Segment tl tr, Segment tr br, Segment tl bl, Segment bl br]

-- | Returns true if the AABB contains the point
containsPoint :: AABB -> Point -> Bool
containsPoint a (V2 x y) = (x > minX && x < maxX) && (y > minY && y < maxY)
  where (V2 minX minY) = a^.minPoint
        (V2 maxX maxY) = a^.maxPoint

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
  where (V2 mdLeft mdBottom) = a^.minPoint - b^.maxPoint
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
penetration a b =
    if containsOrigin minkowski
    then Just (closestPointOnBoundsToPoint (V2 0 0) minkowski)
    else Nothing
  where minkowski = minkowskiDifference a b

-- | Returns true if both shapes overlap
overlapping :: AABB -> AABB -> Bool
overlapping a b = containsOrigin $ minkowskiDifference a b
