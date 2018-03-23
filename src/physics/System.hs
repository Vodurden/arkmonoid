{-# LANGUAGE DataKinds     #-}

module Physics.System where

import Types
import Extra.List
import Extra.Ecstasy
import Extra.Maybe
import Extra.V2
import Physics.Shape.Types
import qualified Physics.Shape.Collision as Collision
import qualified Physics.Shape.Segment as Segment
import qualified Physics.Shape.AABB as AABB
import qualified Physics.Shape.Boundary as Boundary

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
    collisions <- stepMovement delta
    stepBounce collisions
    clearImpulse
  where
    clearImpulse = emap $ do
      with impulse
      pure $ defEntity' { impulse = Set 0 }

stepMovement :: Float -> GameSystem (Map.Map Ent [Collision])
stepMovement delta = do
    models <- collisionModels
    entCollisions <- allEntCollisions models
    boundCollisions <- allBoundaryCollisions models
    let allCollisions = Map.unionWith (++) entCollisions boundCollisions
    let activeCollisions = Map.map prioritisedCollisions allCollisions
    entMove delta activeCollisions
    pure activeCollisions
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

    allBoundaryCollisions :: Map.Map Ent CollisionModel -> GameSystem (Map.Map Ent [Collision])
    allBoundaryCollisions models = do
      collisions <- efor $ \ent -> do
        model <- entCollisionModel delta
        let bCollisions = catMaybes $ fmap (\b -> Boundary.collision b model) boundaries
        pure (ent, bCollisions)

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
    entMove _ collisions = emap $ do
      ent <- get entId
      entMovement delta (fromMaybe [] $ Map.lookup ent collisions)


stepBounce :: Map.Map Ent [Collision] -> GameSystem ()
stepBounce collisions = emap $ do
    with bouncy
    ent <- get entId
    let entCollisions = fromMaybe [] $ Map.lookup ent collisions
    vel <- get velocity
    let newVel = foldr reflectByCollision vel entCollisions

    pure defEntity' { velocity = Set newVel }
  where
    reflectByCollision :: Collision -> V2 Float -> V2 Float
    reflectByCollision (PenetrationCollision penVector) v = reflect v penVector
    reflectByCollision (PointCollision _ collisionSegment _) v = reflect v (Segment.normalVector collisionSegment)

boundaries :: [Boundary]
boundaries =
    [ Boundary (Line topLeftCorner topRightCorner) BoundLeft
    , Boundary (Line bottomLeftCorner bottomRightCorner) BoundRight
    , Boundary (Line bottomLeftCorner topLeftCorner) BoundLeft
    , Boundary (Line bottomRightCorner topRightCorner) BoundRight
    ]
  where
    halfScreenWidth   = (fromIntegral screenWidth) / 2
    halfScreenHeight  = (fromIntegral screenHeight) / 2
    topLeftCorner     = V2 (-halfScreenWidth) (halfScreenHeight)
    topRightCorner    = V2 (halfScreenWidth) (halfScreenHeight)
    bottomLeftCorner  = V2 (-halfScreenWidth) (-halfScreenHeight)
    bottomRightCorner = V2 (halfScreenWidth) (-halfScreenHeight)

entMovement :: (Monad m) => Float -> [Collision] -> GameQueryT m (Entity' 'SetterOf)
entMovement delta [] = do
  without frozen
  mov <- entNormalMovement delta
  p <- get position
  pure defEntity'
    { position = Set $ p + (fromMaybe (V2 0 0) mov)
    }
entMovement _ collisions = do
    p <- get position
    let resolveV = foldr (+) (V2 0 0) $ fmap (resolutionVector p) collisions
    pure defEntity'
      { position = Set (p + resolveV)
      }
  where
    resolutionVector :: Point -> Collision -> V2 Float
    resolutionVector _ (PenetrationCollision penVector) = penVector
    resolutionVector pos (PointCollision point _ _) = point - pos

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
