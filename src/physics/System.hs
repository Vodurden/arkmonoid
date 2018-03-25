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
import qualified Data.Map.Strict as Map
import Data.Ecstasy
import Data.Maybe
import Data.List
import Linear.V2
import Linear.Metric
import Linear.Epsilon

step :: Float -> GameSystem (Map.Map Ent [Collision])
step delta = do
    collisions <- stepMovement delta
    stepBounce collisions
    clearImpulse

    pure collisions
  where
    clearImpulse = emap $ do
      with impulse
      pure $ defEntity' { impulse = Set 0 }

stepMovement :: Float -> GameSystem (Map.Map Ent [Collision])
stepMovement delta = do
    models <- collisionModels
    let entCollisions = Collision.runModelCollisions EntCollision models
    let boundCollisions = Collision.runBoundaryCollisions BoundaryCollision boundaries models
    let allCollisions = Map.unionWith (++) entCollisions boundCollisions
    let activeCollisions = Map.map prioritisedCollisions allCollisions
    moveEnts delta activeCollisions
    pure activeCollisions
  where
    collisionModels :: GameSystem (Map.Map Ent CollisionModel)
    collisionModels = do
      models <- efor $ \ent -> do
        model <- entCollisionModel delta
        pure (ent, model)

      pure $ Map.fromList models

    -- | Prioritises collisions by the following rules:
    -- |
    -- |   1. If there are no collisions: just move
    -- |   2. If there are any penetration collisions: resolve them all since we're colliding
    -- |   3. Otherwise, if there are point collisions: resolve the shortest one
    prioritisedCollisions :: [Collision] -> [Collision]
    prioritisedCollisions collisions =
      let penetrationCollisions =
            filter (Collision.onCollisionType Collision.isPenetrationCollision) collisions
          pointCollision = take 1
            $ sortOn
              (Collision.onCollisionType (\(PointCollision _ _ frameMovement) -> frameMovement))
            $ filter (Collision.onCollisionType Collision.isPointCollision) collisions
      in penetrationCollisions `ifNonEmptyElse` pointCollision

    moveEnts :: Float -> Map.Map Ent [Collision] -> GameSystem ()
    moveEnts _ collisions = emap $ do
      ent <- get entId
      entMovement delta (fromMaybe [] $ Map.lookup ent collisions)

-- | Returns all the collisions that occurred between entities.
-- |
-- | This includes collisions from both entities perspectives so
-- | all collisions will incur two entries in this list.
-- |
-- | The first entity is the entity that collided with the second
-- | entity.
collidingEnts :: Map.Map Ent [Collision] -> [(Ent, Ent)]
collidingEnts collisions =
    concatMap (uncurry entCollisions) $ Map.toList collisions
  where
    entCollisions :: Ent -> [Collision] -> [(Ent, Ent)]
    entCollisions ent cs = catMaybes $ fmap (entCollision ent) cs

    entCollision :: Ent -> Collision -> Maybe (Ent, Ent)
    entCollision ent (EntCollision e _) = Just (ent, e)
    entCollision _ (BoundaryCollision _) = Nothing


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
    reflectByCollision collision v = case Collision.collisionType collision of
      (PenetrationCollision penVector) -> reflect v penVector
      (PointCollision _ collisionSegment _) -> reflect v (Segment.normalVector collisionSegment)

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
  entMove (fromMaybe (V2 0 0) mov)
entMovement _ collisions = do
    p <- get position
    let resolveV = foldr (+) (V2 0 0) $ fmap (resolutionVector p) collisions
    entMove resolveV
  where
    resolutionVector :: Point -> Collision -> V2 Float
    resolutionVector pos collision = case Collision.collisionType collision of
      (PenetrationCollision penVector) -> penVector
      (PointCollision point _ _) -> point - pos

entCollisionModel :: (Monad m) => Float -> GameQueryT m CollisionModel
entCollisionModel delta = do
  center <- get position
  (Box w h) <- get geometry
  let halfWH = V2 (w / 2) (h / 2)
  let aabbMin = center - halfWH
  let aabbMax = center + halfWH
  m <- entNormalMovement delta
  let aabb = AABB aabbMin aabbMax
  pure $ case m of
           (Just movement) -> DynamicAABB aabb movement
           Nothing -> StaticAABB aabb

entMove :: (Monad m) => V2 Float -> GameQueryT m (Entity' 'SetterOf)
entMove v = do
  guard (not $ nearZero v)
  p <- get position
  pure defEntity'
    { position = Set (p + v)
    }

entNormalMovement :: (Monad m) => Float -> GameQueryT m (Maybe (V2 Float))
entNormalMovement delta = do
  maybeV <- getMaybe velocity
  let maybeVScaled = (fmap . fmap) (*delta) maybeV
  maybeI <- getMaybe impulse
  pure $ applyOrOther (+) maybeVScaled maybeI
