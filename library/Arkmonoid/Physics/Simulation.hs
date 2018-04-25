{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Arkmonoid.Physics.Simulation where

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Linear.V2

import           Arkmonoid.Physics.Types
import qualified Arkmonoid.Physics.GameObject as GameObject
import           Arkmonoid.Physics.CollisionDetection.Types
import qualified Arkmonoid.Physics.CollisionResolution as CollisionResolution

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
