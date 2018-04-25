module Arkmonoid.Physics.CollisionDetection.GameCollisions where

import           Control.Lens
import qualified Data.Map.Strict as Map

import qualified Arkmonoid.Physics.CollisionDetection.GameCollision as GameCollision
import           Arkmonoid.Physics.CollisionDetection.Types

collidingIds :: GameCollisions id -> [(id, id)]
collidingIds collisions = concatMap GameCollision.idPair $ Map.elems collisions
