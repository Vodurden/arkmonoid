module Simulate.Physics where

import Data.Foldable (for_)
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Ecstasy
import Linear.V2

import Types
import Simulate.Collision as Collision
import qualified Simulate.Shape as S

stepPhysics :: Float -> GameSystem ()
stepPhysics delta = do
  stepVelocity delta
  allCollisions <- findCollisions
  traverse_ resolveCollision allCollisions

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

resolveCollision :: Collision -> GameSystem ()
resolveCollision (BoundaryCollision ent impact) = resolveImpact ent impact
resolveCollision (EntityCollision ent1 ent2 impact1 impact2) =
  (resolveImpact ent1 impact1) >> (resolveImpact ent2 impact2)

resolveImpact :: Ent -> Impact -> GameSystem ()
resolveImpact ent (Impact pen) = do
  cs <- getEntity ent
  sets <- flip unQueryT cs $ do
    pos <- get position
    vel <- get velocity
    (Box w h) <- get geometry
    let newPos = pos + pen

    pure defEntity'
      { position = Set newPos
      }

  for_ sets $ setEntity ent
