{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Physics.Simulation where

import           Physics.Types
import qualified Physics.GameObject as GameObject
import           Physics.CollisionDetection.Types
import qualified Physics.CollisionResolution as CollisionResolution

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Linear.V2

-- | Simulates movement and resolves collisions for a single frame of the world.
simulate :: (Ord id) => Float -> GameCollisions id -> [GameObject id] -> [GameObject id]
simulate delta collisions = fmap (simulateObject delta collisions)

simulateObject :: forall id. (Ord id) => Float -> GameCollisions id -> GameObject id -> GameObject id
simulateObject delta collisions obj = clearImpulse $ moveOrCollide obj
  where
    moveOrCollide :: GameObject id -> GameObject id
    moveOrCollide =
      case Map.lookup (obj^.identifier) collisions of
        (Just collision) -> CollisionResolution.resolveCollision collision
        Nothing -> GameObject.moveObject delta

    clearImpulse :: GameObject id -> GameObject id
    clearImpulse = set (physical.impulse) (V2 0 0)
