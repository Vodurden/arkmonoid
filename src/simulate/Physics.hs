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
step :: (Ent -> Ent -> GameSystem ())    -- ^ onEntityCollision
     -> Float                            -- ^ The time (in milliseconds) elapsed since the last frame
     -> GameSystem ()
step onEntityCollision delta = do
    clearSpeed
    stepVelocity delta
    stepImpulse
    allCollisions <- collisions (fromIntegral screenWidth) (fromIntegral screenHeight)
    traverse_ (resolveCollision onCollision onEntityCollision) allCollisions
  where
    onCollision :: Ent -> Impact -> GameSystem ()
    onCollision ent impact = do
      resolveOverlap ent impact
      resolveBounce ent impact

clearSpeed :: GameSystem ()
clearSpeed = emap $ pure defEntity' { speed = Unset }

stepVelocity :: Float -> GameSystem ()
stepVelocity delta =
  emap $ do
    without frozen
    p <- get position
    v <- get velocity
    let scaledV = fmap (*delta) v
    pure defEntity'
      { position = Set (p + scaledV)
      , speed = Set scaledV
      }

-- | Impulse is applied in it's entirety in a single frame.
-- |
-- | This is useful to allow non-physics based systems to apply changes
-- | in position
stepImpulse :: GameSystem ()
stepImpulse = emap $ do
  without frozen
  p <- get position
  i <- get impulse
  s <- getMaybe speed
  pure defEntity'
    { position = Set (p + i)
    , impulse = Unset
    , speed = Set $ (fromMaybe (V2 0 0) s) + i
    }

resolveCollision :: (Ent -> Impact -> GameSystem a)
                 -> (Ent -> Ent -> GameSystem a)
                 -> Collision
                 -> GameSystem a
resolveCollision resolve _ (BoundaryCollision ent impact) = resolve ent impact
resolveCollision resolve resolveEnt (EntityCollision ent1 ent2 impact1 impact2) =
  resolve ent1 impact1 >>
  resolve ent2 impact2 >>
  resolveEnt ent1 ent2

-- | Moves a colliding entity such that it is no longer colliding.
resolveOverlap :: Ent -> Impact -> GameSystem ()
resolveOverlap ent (Impact pen _) = forEnt ent $ do
  pos <- get position
  let newPos = pos + pen

  pure defEntity'
    { position = Set newPos }

-- | Changes the velocity of a colliding entity such that it "bounces" off the thing it
-- | collided with. Only applies to entities that are bouncy
resolveBounce :: Ent -> Impact -> GameSystem ()
resolveBounce ent (Impact pen impactVel) = forEnt ent $ do
  with bouncy
  vel <- get velocity
  let newVel = (reflect vel pen) + impactVel

  pure defEntity'
    { velocity = Set newVel }
