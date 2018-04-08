{-# LANGUAGE DataKinds     #-}

-- | The PhysicsSystem simulates the physics of the game including motion and
-- | collision detection.
-- |
-- | It is the entry point for all of the physics behavior in the game.
-- |
-- | The physics system is broken up into a few major phases:
-- |
-- |   1. Querying the game engine for all entities that need to be
-- |      simulated and constructing a physical model from the
-- |      entities to provide to the physics simulation
-- |
-- |   2. Detecting collisions. This phase includes detectings collisions
-- |      that already exist in the world, such as overlapping entities.
-- |      It also detects collisions that will occur this frame and provides
-- |      information about how to resolve the collision
-- |
-- |   3. Simulating a frame. This phase involves updating the physical model
-- |      of each object. Typically this will include position and velocity
-- |      changes.
-- |
-- |      If a collision was detected in the previous phase then the resolution
-- |      of that collision is also handled during this step.
-- |
-- |   4. Updating the game state. Now that we know where all of our objects should
-- |      end up we need to update the external game state with our changes.
-- |
-- |      We also return the collisions we detected so other systems can respond to
-- |      collisions.
-- |
module Physics.PhysicsSystem where

import           Extra.Ecstasy
import           Physics.Types
import qualified Physics.CollisionDetection.Detection as CollisionDetection
import           Physics.CollisionDetection.Types
import           Physics.Shape.Types
import qualified Physics.Simulation as Simulation
import           Types

import Control.Lens
import Data.Ecstasy
import Data.Foldable
import Data.Map (Map)
import Linear.V2

step :: Float -> GameSystem (Map Ent (GameCollision Ent))
step delta = do
    objects <- collisionObjects
    let collisions = CollisionDetection.collisions delta boundaries objects
    let newObjects = Simulation.simulate delta collisions objects
    updateEntities newObjects
    pure collisions
  where
    boundaries :: [Boundary]
    boundaries =
        [ LeftBoundary (Line topLeftCorner topRightCorner) Solid
        , RightBoundary (Line bottomLeftCorner bottomRightCorner) Solid
        , LeftBoundary (Line bottomLeftCorner topLeftCorner) Solid
        , RightBoundary (Line bottomRightCorner topRightCorner) Solid
        ]
      where
        halfScreenWidth   = (fromIntegral screenWidth) / 2
        halfScreenHeight  = (fromIntegral screenHeight) / 2
        topLeftCorner     = V2 (-halfScreenWidth) (halfScreenHeight)
        topRightCorner    = V2 (halfScreenWidth) (halfScreenHeight)
        bottomLeftCorner  = V2 (-halfScreenWidth) (-halfScreenHeight)
        bottomRightCorner = V2 (halfScreenWidth) (-halfScreenHeight)

collisionObjects :: GameSystem [GameObject Ent]
collisionObjects = do
    efor $ \ent -> entCollisionObject ent
  where
    entCollisionObject :: (Monad m) => Ent -> GameQueryT m (GameObject Ent)
    entCollisionObject ent = do
      physObject <- get physicalObject
      pure $ GameObject ent physObject

updateEntities :: [GameObject Ent] -> GameSystem ()
updateEntities = traverse_ updateEntity
  where updateEntity :: GameObject Ent -> GameSystem ()
        updateEntity obj = forEnt (obj^.identifier) $ do
          pure $ defEntity'
            { physicalObject = Set $ obj^.physical
            }