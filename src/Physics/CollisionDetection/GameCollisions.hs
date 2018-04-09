module Physics.CollisionDetection.GameCollisions where

import qualified Physics.CollisionDetection.GameCollision as GameCollision
import           Physics.CollisionDetection.Types

import           Control.Lens
import qualified Data.Map.Strict as Map

collidingIds :: GameCollisions id -> [(id, id)]
collidingIds collisions = concatMap GameCollision.idPair $ Map.elems collisions
