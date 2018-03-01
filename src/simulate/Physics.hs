module Simulate.Physics where

import Data.Foldable (for_)
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Ecstasy
import Linear.V2

import Types
import Extra.V2
import Extra.Ecstasy
import Simulate.Collision
import qualified Simulate.Shape as S

-- | Step the physical simulation one frame.
-- |
-- | This step includes motion, collision detection and collision resolution
stepPhysics :: Float                            -- ^ The time (in milliseconds) elapsed since the last frame
            -> GameSystem ()
stepPhysics delta = do
    stepVelocity delta
    allCollisions <- collisions (fromIntegral screenWidth) (fromIntegral screenHeight)
    traverse_ (resolveCollision onCollision) allCollisions
  where
    onCollision :: Ent -> Impact -> GameSystem ()
    onCollision ent impact = do
      resolveOverlap ent impact
      resolveBounce ent impact

stepVelocity :: Float -> GameSystem ()
stepVelocity delta =
  emap $ do
    p <- get position
    v <- get velocity
    let scaledV = fmap (*delta) v
    pure defEntity'
      { position = Set (p + scaledV) }

resolveCollision :: (Ent -> Impact -> GameSystem a) -> Collision -> GameSystem a
resolveCollision resolve (BoundaryCollision ent impact) = resolve ent impact
resolveCollision resolve (EntityCollision ent1 ent2 impact1 impact2) =
  resolve ent1 impact1 >> resolve ent2 impact2

-- | Moves a colliding entity such that it is no longer colliding.
resolveOverlap :: Ent -> Impact -> GameSystem ()
resolveOverlap ent (Impact pen) = forEnt ent $ do
  pos <- get position
  let newPos = pos + pen

  pure defEntity'
    { position = Set newPos }

-- | Changes the velocity of a colliding entity such that it "bounces" off the thing it
-- | collided with. Only applies to entities that are bouncy
resolveBounce :: Ent -> Impact -> GameSystem ()
resolveBounce ent (Impact pen) = forEnt ent $ do
  with bouncy
  vel <- get velocity
  let newVel = reflect vel pen

  pure defEntity'
    { velocity = Set newVel }
