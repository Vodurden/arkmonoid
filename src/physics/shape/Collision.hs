module Physics.Shape.Collision where

import Physics.Shape.Types
import Physics.Shape.CollisionModel
import qualified Physics.Shape.AABB as AABB
import qualified Physics.Shape.Segment as Segment

import Data.Maybe
import Data.List
import Linear.V2
import Linear.Metric

-- | Finds all the collisions between a given model and the rest of the models
collisions :: CollisionModel -> [CollisionModel] -> [Collision]
collisions model models = catMaybes $ fmap (collision model) models

-- | Returns true if the shapes are currently overlapping.
colliding :: CollisionModel -> CollisionModel -> Bool
colliding cm1 cm2 = AABB.overlapping (box cm1) (box cm2)

-- | Find a collision between two models
collision :: CollisionModel -> CollisionModel -> Maybe Collision
collision a b | colliding a b = staticCollision a b
              | otherwise = dynamicCollision a b

-- | Detects collisions between shapes that will collide this frame
dynamicCollision :: CollisionModel -> CollisionModel -> Maybe Collision
dynamicCollision aabb1 aabb2 = do
    let relativeMotion = movement aabb1 - movement aabb2
    let collisionSegment = Segment (V2 0 0) relativeMotion
    let minkowski = AABB.minkowskiDifference (box aabb2) (box aabb1)
    (PointCollision intersectedPoint intersectedSegment intersectionDistance) <-
      staticSegmentAABBCollision collisionSegment minkowski
    minkowskiIntersectionFraction <- Segment.intersectionFraction collisionSegment intersectedPoint

    let collisionMovement = fmap (*minkowskiIntersectionFraction) (movement aabb1)

    -- This is an ugly hack to stop velocity-based collision from getting stuck on various
    -- bricks.
    let slightlyShorterCollisionMovement = fmap (*0.98) collisionMovement
    let collisionPoint = (AABB.center (box aabb1)) + slightlyShorterCollisionMovement
    pure $ PointCollision collisionPoint intersectedSegment intersectionDistance

-- | Detects collisions between shapes that are _already_ colliding.
-- |
-- | The returned collision is from the perspective of the first shape
staticCollision :: CollisionModel -> CollisionModel -> Maybe Collision
staticCollision aabb1 aabb2 = fmap PenetrationCollision (staticAABBCollision (box aabb1) (box aabb2))

-- | Detects collision between two AABB's if they are already colliding
staticAABBCollision :: AABB -> AABB -> Maybe PenetrationVector
staticAABBCollision = AABB.penetration

-- | Detects a collision between two line segments if they are already colliding
staticSegmentCollision :: Segment -> Segment -> Maybe Collision
staticSegmentCollision segA segB = do
  point <- Segment.intersection segA segB
  pure $ PointCollision point segB (distance (Segment.start segA) point)

-- | Detects a collision between a line segment and an AABB if they are already colliding
staticSegmentAABBCollision :: Segment -> AABB -> Maybe Collision
staticSegmentAABBCollision segment aabb =
  let intersections = catMaybes $ fmap (staticSegmentCollision segment) (AABB.segments aabb)
      smallestIntersection = listToMaybe
        $ sortOn (\(PointCollision point _ _) -> distance (Segment.start segment) point) intersections
  in smallestIntersection

isPointCollision :: Collision -> Bool
isPointCollision (PointCollision _ _ _) = True
isPointCollision _ = False

isPenetrationCollision :: Collision -> Bool
isPenetrationCollision (PenetrationCollision _) = True
isPenetrationCollision _ = False
