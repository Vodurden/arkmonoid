module Physics.CollisionDetection.GameCollisions where

import Physics.CollisionDetection.Types

import Control.Lens
import qualified Data.Map.Strict as Map

collidingIds :: GameCollisions id -> [(id, id)]
collidingIds collisions =
    concatMap idPair $ Map.elems collisions
  where
    idPair :: GameCollision id -> [(id, id)]
    idPair (GBoundaryCollision _ _) = []
    idPair (GExistingCollision _ cs) = fmap (\e -> (e^.objectId, e^.targetId)) cs
    idPair (GImpendingCollision _ impending) = [(impending^.objectId, impending^.targetId)]
