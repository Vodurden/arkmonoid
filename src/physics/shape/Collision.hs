{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Physics.Shape.Collision where

import Physics.Shape.Types
import Physics.Shape.CollisionModel
import qualified Physics.Shape.AABB as AABB
import qualified Physics.Shape.Segment as Segment
import qualified Physics.Shape.Boundary as Boundary

import Data.Maybe
import Data.Function
import Data.List
import Extra.List
import qualified Data.Map.Strict as Map
import Linear.V2
import Linear.Metric

-- | Finds all the collisions between all given models. Also allows for tracking of the "key" of
-- | each model to allow attribution of collisions to keys
runModelCollisions :: forall key a. Ord key
                   => (key -> CollisionType -> a)
                   -> Map.Map key CollisionModel
                   -> Map.Map key [a]
runModelCollisions toCollision models =
    let collisionPairs = pairs $ Map.toList models

        collisionTypeMaybes :: [((key, key), Maybe CollisionType)]
        collisionTypeMaybes = flip concatMap collisionPairs $ \((e1, cm1), (e2, cm2)) ->
          [((e1, e2), collision cm1 cm2), ((e2, e1), collision cm2 cm1)]

        collisionTypes :: [((key, key), [CollisionType])]
        collisionTypes = [ ((k1, k2), [ct]) | ((k1, k2), Just ct) <- collisionTypeMaybes ]

        collisions :: [(key, [a])]
        collisions = [ (key, fmap (toCollision key2) cs) | ((key, key2), cs) <- collisionTypes]
    in Map.fromListWith (++) collisions

-- | Finds all the collisions between the given models and the given boundaries.
runBoundaryCollisions :: forall key a. Ord key
                      => (CollisionType -> a)
                      -> [Boundary]
                      -> Map.Map key CollisionModel
                      -> Map.Map key [a]
runBoundaryCollisions toCollision boundaries models =
  flip fmap models $ \model ->
    fmap toCollision $ catMaybes $ fmap (\b -> Boundary.collision b model) boundaries

-- | Find a collision between two models
collision :: CollisionModel -> CollisionModel -> Maybe CollisionType
collision a @ (StaticAABB _) b @ (StaticAABB _) = staticCollision a b
collision a b | colliding a b = staticCollision a b
              | otherwise = dynamicCollision a b

-- | Returns true if the shapes are currently overlapping.
colliding :: CollisionModel -> CollisionModel -> Bool
colliding cm1 cm2 = AABB.overlapping (box cm1) (box cm2)

-- | Detects collisions between shapes that will collide this frame
dynamicCollision :: CollisionModel -> CollisionModel -> Maybe CollisionType
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
staticCollision :: CollisionModel -> CollisionModel -> Maybe CollisionType
staticCollision aabb1 aabb2 = fmap PenetrationCollision (staticAABBCollision (box aabb1) (box aabb2))

-- | Detects collision between two AABB's if they are already colliding
staticAABBCollision :: AABB -> AABB -> Maybe PenetrationVector
staticAABBCollision = AABB.penetration

-- | Detects a collision between two line segments if they are already colliding
staticSegmentCollision :: Segment -> Segment -> Maybe CollisionType
staticSegmentCollision segA segB = do
  point <- Segment.intersection segA segB
  pure $ PointCollision point segB (distance (Segment.start segA) point)

-- | Detects a collision between a line segment and an AABB if they are already colliding
staticSegmentAABBCollision :: Segment -> AABB -> Maybe CollisionType
staticSegmentAABBCollision segment aabb =
  let intersections = catMaybes $ fmap (staticSegmentCollision segment) (AABB.segments aabb)
      smallestIntersection = listToMaybe
        $ sortOn (\(PointCollision point _ _) -> distance (Segment.start segment) point) intersections
  in smallestIntersection

collisionType :: Collision -> CollisionType
collisionType = onCollisionType id

onCollisionType :: (CollisionType -> a) -> Collision -> a
onCollisionType f (EntCollision _ ct) = f ct
onCollisionType f (BoundaryCollision ct) = f ct

isPointCollision :: CollisionType -> Bool
isPointCollision (PointCollision _ _ _) = True
isPointCollision _ = False

isPenetrationCollision :: CollisionType -> Bool
isPenetrationCollision (PenetrationCollision _) = True
isPenetrationCollision _ = False
