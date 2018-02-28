module Simulate.Physics where

import Data.Foldable (for_)
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Ecstasy
import Linear.V2

import Types
import Extra.V2
import Simulate.Collision as Collision
import qualified Simulate.Shape as S

stepPhysics :: Float -> GameSystem ()
stepPhysics delta = do
  stepVelocity delta
  allCollisions <- findCollisions
  traverse_ (resolveCollision resolveImpact) allCollisions
  traverse_ (resolveCollision resolveBounce) allCollisions

stepVelocity :: Float -> GameSystem ()
stepVelocity delta =
  emap $ do
    p <- get position
    v <- get velocity
    let scaledV = fmap (*delta) v
    pure defEntity'
      { position = Set (p + scaledV) }

findCollisions :: GameSystem [Collision]
findCollisions = Collision.collisions (fromIntegral screenWidth) (fromIntegral screenHeight)

resolveCollision :: (Ent -> Impact -> GameSystem a) -> Collision -> GameSystem a
resolveCollision resolve (BoundaryCollision ent impact) = resolve ent impact
resolveCollision resolve (EntityCollision ent1 ent2 impact1 impact2) =
  resolve ent1 impact1 >> resolve ent2 impact2

resolveImpact :: Ent -> Impact -> GameSystem ()
resolveImpact ent (Impact pen) = do
  cs <- getEntity ent
  sets <- flip unQueryT cs $ do
    pos <- get position
    let newPos = pos + pen

    pure defEntity'
      { position = Set newPos }

  for_ sets $ setEntity ent

resolveBounce :: Ent -> Impact -> GameSystem ()
resolveBounce ent (Impact pen) = do
  cs <- getEntity ent
  sets <- flip unQueryT cs $ do
    with bouncy
    vel <- get velocity
    let newVel = reflect vel pen

    pure defEntity'
      { velocity = Set newVel }

  for_ sets $ setEntity ent
