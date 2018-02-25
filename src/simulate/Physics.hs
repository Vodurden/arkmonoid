module Simulate.Physics where

import Data.Foldable (for_)
import Control.Monad
import Data.Maybe
import Data.Ecstasy
import Linear.V2

import Types
import Simulate.Collision
import qualified Simulate.Shape as S

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
findCollisions =
  boundaryCollisions (fromIntegral screenWidth) (fromIntegral screenHeight)

resolveCollision :: Collision -> GameSystem ()
resolveCollision (BoundaryCollision ent impact) = resolveImpact ent impact
resolveCollision (EntityCollision ent1 impact1 ent2 impact2) =
  (resolveImpact ent1 impact1) >> (resolveImpact ent2 impact2)

resolveImpact :: Ent -> Impact -> GameSystem ()
resolveImpact ent (Impact side (V2 xImpact yImpact)) = do
  cs <- getEntity ent
  sets <- flip unQueryT cs $ do
    (V2 x y) <- get position
    vel <- get velocity
    (Box w h) <- get geometry
    let newPos = case side of
                   TopSide ->    V2 x                    (yImpact - (h / 2))
                   LeftSide ->   V2 (xImpact + (w / 2))  y
                   BottomSide -> V2 x                    (yImpact + (h / 2))
                   RightSide ->  V2 (xImpact - (w / 2))  y
    let newVel = case side of
                   TopSide ->    vel * (V2 1    (-1))
                   LeftSide  ->  vel * (V2 (-1)   1)
                   BottomSide -> vel * (V2 1    (-1))
                   RightSide  -> vel * (V2 (-1)   1)

    pure defEntity'
      { position = Set newPos
      , velocity = Set newVel
      }

  for_ sets $ setEntity ent
