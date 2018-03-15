{-# LANGUAGE DataKinds     #-}

module Physics.System where

import Types
import Extra.List
import Extra.Ecstasy
import Extra.Maybe
import Extra.V2
import Shape.Types
import qualified Shape.Collision as Collision
import qualified Shape.Segment as Segment
import qualified Shape.AABB as AABB

import Control.Monad
import Control.Applicative
import Data.Foldable
import qualified Data.Map as Map
import Data.Ecstasy
import Data.Maybe
import Data.List
import Linear.V2
import Linear.Metric

step :: Float -> GameSystem ()
step delta = do
    clearSpeed
    stepMovement delta
    clearImpulse
  where
    clearSpeed = emap $ pure defEntity' { speed = Unset }
    clearImpulse = emap $ pure defEntity' { impulse = Unset }

stepMovement :: Float -> GameSystem ()
stepMovement delta = do
    models <- collisionModels
    allCollisions <- allEntCollisions models
    let activeCollisions = Map.map prioritisedCollisions allCollisions
    entMove delta activeCollisions
  where
    collisionModels :: GameSystem (Map.Map Ent CollisionModel)
    collisionModels = do
      models <- efor $ \ent -> do
        model <- entCollisionModel delta
        pure (ent, model)

      pure $ Map.fromList models

    allEntCollisions :: Map.Map Ent CollisionModel -> GameSystem (Map.Map Ent [Collision])
    allEntCollisions models = do
      collisions <- efor $ \ent -> do
        model <- entCollisionModel delta
        let otherModels = Map.delete ent models
        let eCollisions = Collision.collisions model (Map.elems otherModels)
        pure (ent, eCollisions)

      pure $ Map.fromList collisions

    -- | Prioritises collisions by the following rules:
    -- |
    -- |   1. If there are no collisions: just move
    -- |   2. If there are any penetration collisions: resolve them all since we're colliding
    -- |   3. Otherwise, if there are point collisions: resolve the shortest one
    prioritisedCollisions :: [Collision] -> [Collision]
    prioritisedCollisions collisions =
      let penetrationCollisions = filter Collision.isPenetrationCollision collisions
          pointCollision = take 1
            $ sortOn (\(PointCollision _ _ frameMovement) -> frameMovement)
            $ filter Collision.isPointCollision collisions
      in penetrationCollisions `ifNonEmptyElse` pointCollision

    entMove :: Float -> Map.Map Ent [Collision] -> GameSystem ()
    entMove _ collisions = do
      traverse_ (\(ent, c) -> entMovement ent delta c) $ Map.toList collisions

entMovement :: Ent -> Float -> [Collision] -> GameSystem ()
entMovement ent delta [] = forEnt ent $ do
  without frozen
  mov <- entNormalMovement delta
  entMoveBy $ fromMaybe (V2 0 0) mov
entMovement ent _ collisions = traverse_ (onCollision ent) collisions
  where onCollision :: Ent -> Collision -> GameSystem ()
        onCollision e c = do
          onCollisionResolveCollision e c
          onCollisionBounce e c

onCollisionResolveCollision :: Ent -> Collision -> GameSystem ()
onCollisionResolveCollision ent (PenetrationCollision penVector) = forEnt ent $ entMoveBy penVector
onCollisionResolveCollision ent (PointCollision point _ _) = forEnt ent $ entMoveTo point

onCollisionBounce :: Ent -> Collision -> GameSystem ()
onCollisionBounce ent collision = forEnt ent $ do
    with bouncy
    vel <- get velocity
    let newVel = reflect vel reflectionVector

    pure defEntity' { velocity = Set newVel }
  where
    reflectionVector = case collision of
      (PenetrationCollision penVector) -> penVector
      (PointCollision _ collisionSegment _) -> Segment.normalVector collisionSegment

entCollisionModel :: (Monad m) => Float -> GameQueryT m CollisionModel
entCollisionModel delta = do
  p <- get position
  (Box w h) <- get geometry
  m <- entNormalMovement delta
  let aabb = AABB p (V2 w h)
  pure $ case m of
           (Just movement) -> DynamicAABB aabb movement
           Nothing -> StaticAABB aabb

entNormalMovement :: (Monad m) => Float -> GameQueryT m (Maybe (V2 Float))
entNormalMovement delta = do
  maybeV <- getMaybe velocity
  let maybeVScaled = (fmap . fmap) (*delta) maybeV
  maybeI <- getMaybe impulse
  pure $ applyOrOther (+) maybeVScaled maybeI

-- | Move an entity by the given amount
entMoveBy :: (Monad m) => V2 Float -> GameQueryT m (Entity' 'SetterOf)
entMoveBy amount = do
  p <- get position
  pure defEntity'
    { position = Set (p + amount)
    , speed = Set amount
    }

-- | Move an entity to the given point
entMoveTo :: (Monad m) => V2 Float -> GameQueryT m (Entity' 'SetterOf)
entMoveTo point = do
  p <- get position
  let diff = point - p
  entMoveBy diff
