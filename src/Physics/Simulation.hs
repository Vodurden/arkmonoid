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
simulateObject delta collisions = simulateIfUnfrozen
  where
    simulateIfUnfrozen :: GameObject id -> GameObject id
    simulateIfUnfrozen obj =
      if not (obj^.physical.frozen)
      then clearImpulse $ moveOrCollide obj
      else obj

    moveOrCollide :: GameObject id -> GameObject id
    moveOrCollide obj =
      case Map.lookup (obj^.identifier) collisions of
        (Just collision) -> CollisionResolution.resolveCollision collision obj
        Nothing -> GameObject.moveObject delta obj

    clearImpulse :: GameObject id -> GameObject id
    clearImpulse = set (physical.impulse) (V2 0 0)
