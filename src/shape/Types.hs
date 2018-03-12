module Shape.Types where

import Linear.V2

type Point = V2 Float
type Size = V2 Float
type FrameMovement = V2 Float

-- | An infinite line passing through the given points
data Line = Line Point Point
  deriving Show

-- | A bounded line with a start point and end point
data Segment = Segment Point Point
  deriving Show

-- | An axis-aligned bounding box with a center point and size
data AABB = AABB Point Size
  deriving Show

data CollisionModel = DynamicAABB AABB FrameMovement
                    | StaticAABB AABB
  deriving Show

-- | Represents a collision from the perspective of a single shape.
data Collision = PenetrationCollision PenetrationVector -- A collision based on the depth of overlap
               | PointCollision Point Segment -- A collision at an exact point with a surface segment
  deriving Show

-- | Represents the depth of penetration between two colliding shapes.
-- |
-- | When added to the position of the colliding shape this will resolve
-- | the collision
type PenetrationVector = (V2 Float)
