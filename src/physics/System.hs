{-# LANGUAGE DataKinds     #-}

module Physics.System where

import Types
import Extra.List
import Extra.Ecstasy
import Extra.Maybe
import Extra.V2
import Shape.Types
import qualified Shape.Collision as Collision

import Control.Monad
import Control.Applicative
import Data.Foldable
import qualified Data.Map as Map
import Data.Ecstasy
import Data.Maybe
import Linear.V2

step :: Float -> GameSystem ()
step delta = do
  stepMovement delta

stepMovement :: Float -> GameSystem ()
stepMovement delta = do
  collisionModels <- efor $ \ent -> do
    model <- entCollisionModel delta
    pure (ent, model)

  let collisionMap = Map.fromList collisionModels

  emapIndexed $ \ent -> entCollisionMovement ent delta collisionMap <|> entFreeMovement delta
  emap $ pure defEntity' { impulse = Unset }

-- | Move an entity that has a collision model. Other physical entities will affect
-- | this movement.
entCollisionMovement :: (Monad m)
                     => Ent
                     -> Float
                     -> Map.Map Ent CollisionModel
                     -> GameQueryT m (Entity' 'SetterOf)
entCollisionMovement ent delta collisionModels = do
    without frozen
    model <- entCollisionModel delta
    let otherModels = Map.delete ent collisionModels
    let collisions = Collision.collisions model (Map.elems otherModels)

    handleCollision collisions
  where
    -- TODO: Multiple collisions
    handleCollision [] = entFreeMovement delta
    handleCollision [collision] =
      (onCollisionResolveCollision collision) >> (onCollisionBounce collision)
    handleCollision _ = undefined

onCollisionResolveCollision :: (Monad m) => Collision -> GameQueryT m (Entity' 'SetterOf)
onCollisionResolveCollision (PenetrationCollision penVector) = entMoveBy penVector
onCollisionResolveCollision (PointCollision point _) = entMoveTo point

onCollisionBounce :: (Monad m) => Collision -> GameQueryT m (Entity' 'SetterOf)
onCollisionBounce collision = do
    with bouncy
    vel <- get velocity
    let newVel = reflect vel reflectionVector

    pure defEntity' { velocity = Set newVel }
  where
    reflectionVector = case collision of
      (PenetrationCollision penVector) -> penVector
      -- TODO: Actual bouncing based on the surface of the point collision
      (PointCollision _ _) -> V2 0 (-1)

entCollisionModel :: (Monad m) => Float -> GameQueryT m CollisionModel
entCollisionModel delta = do
  p <- get position
  (Box w h) <- get geometry
  m <- entNormalMovement delta
  let aabb = AABB p (V2 w h)
  pure $ case m of
           (Just movement) -> DynamicAABB aabb movement
           Nothing -> StaticAABB aabb

-- | Move this entity without regard for it's collision model (if any).
entFreeMovement :: (Monad m) => Float -> GameQueryT m (Entity' 'SetterOf)
entFreeMovement delta = do
  without frozen
  mov <- entNormalMovement delta
  entMoveBy $ fromMaybe (V2 0 0) mov

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
