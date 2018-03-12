module Shape.Collision where

import Shape.Types
import qualified Shape.AABB as AABB
import qualified Shape.Segment as Segment

import Data.Maybe
import Data.List
import Linear.V2
import Linear.Metric

collisions :: CollisionModel -> [CollisionModel] -> [Collision]
collisions model models = catMaybes $ fmap (collision model) models

-- | Returns true if the shapes are currently overlapping.
colliding :: CollisionModel -> CollisionModel -> Bool
colliding cm1 cm2 = AABB.overlapping (box cm1) (box cm2)

collision :: CollisionModel -> CollisionModel -> Maybe Collision
collision a b | colliding a b = staticCollision a b
              | otherwise = dynamicCollision a b

-- | Detects collisions between shapes that will collide this frame
dynamicCollision :: CollisionModel -> CollisionModel -> Maybe Collision
dynamicCollision aabb1 aabb2 = do
    let relativeMotion = movement aabb1 - movement aabb2
    let collisionSegment = Segment (V2 0 0) relativeMotion
    let minkowski = AABB.minkowskiDifference (box aabb2) (box aabb1)
    (intersectionPoint, intersectedSegment) <- staticSegmentAABBCollision collisionSegment minkowski
    minkowskiIntersectionFraction <- Segment.intersectionFraction collisionSegment intersectionPoint

    -- minkowskiCollisionPoint <- staticSegmentAABBCollision collisionSegment minkowski
    -- let fullMovementLength = distance (V2 0 0) relativeMotion
    -- let allowedMovementLength = distance (V2 0 0) minkowskiCollisionPoint
    -- let movementFraction = allowedMovementLength / fullMovementLength
    let collisionMovement = fmap (*minkowskiIntersectionFraction) (movement aabb1)
    let collisionPoint = (AABB.center (box aabb1)) + collisionMovement
    pure $ PointCollision collisionPoint intersectedSegment
  where
    relativeMotion = movement aabb1 - movement aabb2

    collisionSegment = Segment (V2 0 0) relativeMotion
    minkowski = AABB.minkowskiDifference (box aabb2) (box aabb1)

-- | Detects collisions between shapes that are _already_ colliding.
-- |
-- | The returned collision is from the perspective of the first shape
staticCollision :: CollisionModel -> CollisionModel -> Maybe Collision
staticCollision aabb1 aabb2 = fmap PenetrationCollision (staticAABBCollision (box aabb1) (box aabb2))

-- | Detects collision between two AABB's that are already colliding
staticAABBCollision :: AABB -> AABB -> Maybe PenetrationVector
staticAABBCollision = AABB.penetration

staticSegmentCollision :: Segment -> Segment -> Maybe Point
staticSegmentCollision = Segment.intersection

staticSegmentAABBCollision :: Segment -> AABB -> Maybe (Point, Segment)
staticSegmentAABBCollision segment aabb =
  let intersect :: Segment -> Segment -> Maybe (Point, Segment)
      intersect input collisionSurface = do
        point <- Segment.intersection input collisionSurface
        pure (point, collisionSurface)

      intersections = catMaybes $ fmap (intersect segment) (AABB.segments aabb)
      smallestIntersection = listToMaybe
        $ sortOn (distance (Segment.start segment) . fst) intersections
  in smallestIntersection


movement :: CollisionModel -> V2 Float
movement (DynamicAABB _ m) = m
movement (StaticAABB _) = (V2 0 0)

box :: CollisionModel -> AABB
box (DynamicAABB aabb _) = aabb
box (StaticAABB aabb) = aabb
