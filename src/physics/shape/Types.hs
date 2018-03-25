module Physics.Shape.Types where

import Linear.V2
import Data.Ecstasy

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
data AABB = AABB Point Point
  deriving Show

data CollisionModel = DynamicAABB AABB FrameMovement
                    | StaticAABB AABB
  deriving Show

data BoundarySide = BoundLeft | BoundRight
  deriving Show

data Boundary = Boundary Line BoundarySide
  deriving Show

-- | Represents the type of collison that is occurring
-- |
-- | PenetrationCollision: A collison based on the depth of overlap. Usually occurs if the objects
-- |                       are already colliding. Includes a penetration vector that will
-- |                       resolve this collision.
-- |
-- | PointCollision: A collision that will occur at a given point. Usually occurs if the objects
-- |                 are not colliding but will collide this frame. Includes the point of collision
-- |                 and a segment representing the surface on which the collision occured.
data CollisionType = PenetrationCollision PenetrationVector
                   | PointCollision Point Segment Float
  deriving Show

-- | Represents a collision from the perspective of a single shape.
data Collision = EntCollision Ent CollisionType
               | BoundaryCollision CollisionType
  deriving Show

-- | Represents the depth of penetration between two colliding shapes.
-- |
-- | When added to the position of the colliding shape this will resolve
-- | the collision
type PenetrationVector = (V2 Float)
