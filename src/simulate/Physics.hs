module Simulate.Physics where

import Data.Foldable (for_)
import Control.Monad
import Data.Maybe
import Data.Ecstasy
import Linear.V2

import Types

data Direction = North | South | East | West
  deriving (Show, Eq)

data Impact = Impact      -- | a collision from the perspective of a single entity
              Ent         -- ^ The entity that collided
              Direction   -- ^ The side of the entity that was collided
              (V2 Float)  -- ^ The location of the collision
  deriving (Show, Eq)

-- | Describes a collision between two entities, or between a single entity and the boundary
-- | of the world
data Collision = EntityCollision Impact Impact
               | BoundaryCollision Impact
               deriving (Show, Eq)

stepPhysics :: Float -> GameSystem ()
stepPhysics delta = do
  stepVelocity delta
  collisions <- findCollisions
  traverse resolveCollision collisions
  pure ()

stepVelocity :: Float -> GameSystem ()
stepVelocity delta =
  emap $ do
    p <- get position
    v <- get velocity
    let scaledV = fmap (*delta) v
    pure defEntity'
      { position = Set (p + scaledV) }

findCollisions :: GameSystem [Collision]
findCollisions = findBoundaryCollisions

findBoundaryCollisions :: GameSystem [Collision]
findBoundaryCollisions =
  fmap concat $ efor (\ent ->  do
    (V2 x y) <- get position
    (Rectangle w h) <- get geometry

    let left = x - (w / 2)
    let leftWall = (-1) * (fromIntegral screenWidth / 2)
    let leftCollision = if left < leftWall
                        then Just $ Impact ent West (V2 leftWall y)
                        else Nothing

    let right = x + (w / 2)
    let rightWall = fromIntegral screenWidth / 2
    let rightCollision = if right > rightWall
                         then Just $ Impact ent East (V2 rightWall y)
                         else Nothing

    let top = y + (h / 2)
    let topWall = fromIntegral screenHeight / 2
    let topCollision = if top > topWall
                       then Just $ Impact ent North (V2 x topWall)
                       else Nothing

    let bottom = y - (h / 2)
    let bottomWall = (-1) * ((fromIntegral screenHeight) / 2)
    let bottomCollision = if bottom < bottomWall
                          then Just $ Impact ent South (V2 x bottomWall)
                          else Nothing

    let collisions = [leftCollision, rightCollision, topCollision, bottomCollision]

    pure $ fmap BoundaryCollision $ catMaybes collisions)

resolveCollision :: Collision -> GameSystem ()
resolveCollision (BoundaryCollision impact) = resolveImpact impact
resolveCollision (EntityCollision impact1 impact2) =
  (resolveImpact impact1) >> (resolveImpact impact2)

resolveImpact :: Impact -> GameSystem ()
resolveImpact (Impact ent direction (V2 xImpact yImpact)) = do
  cs <- getEntity ent
  sets <- flip unQueryT cs $ do
    (V2 x y) <- get position
    vel <- get velocity
    (Rectangle w h) <- get geometry
    let newPos = case direction of
                   North -> V2 x                    (yImpact - (h / 2))
                   East ->  V2 (xImpact - (w / 2))  y
                   South -> V2 x                    (yImpact + (h / 2))
                   West ->  V2 (xImpact + (w / 2))  y
    let newVel = case direction of
                   North -> vel * (V2 1    (-1))
                   East  -> vel * (V2 (-1)   1)
                   South -> vel * (V2 1    (-1))
                   West  -> vel * (V2 (-1)   1)

    pure defEntity'
      { position = Set newPos
      , velocity = Set newVel
      }

  for_ sets $ setEntity ent

  -- Move position out of collision state
  -- Bounce velocity based on current velocity


    -- let xCollision = case (x, y) of
    --       (x, y) | x < leftWall  -> Just $ Impact ent (V2 leftWall y)
    --              | x > rightWall -> Just $ Impact ent (V2 rightWall y)
    --              | otherwise     -> Nothing
    -- let yCollision = case (x, y) of
    --       (x, y) | y < bottomWall  -> Just $ Impact ent (V2 x bottomWall)
    --              | y > topWall     -> Just $ Impact ent (V2 x topWall)
    --              | otherwise       -> Nothing
