module Physics.CollisionDetection.ExistingCollision where

import Physics.Types
import Physics.CollisionDetection.Types
import qualified Physics.Shape.AABB as AABB

import Control.Lens

-- | Game wrapper for the `collision` function
gameCollision :: GameObject id -> GameObject id -> Maybe (GameExistingCollision id)
gameCollision obj tgt = do
  existing <- collision (obj^.physical) (tgt^.physical)
  pure $ GameExistingCollision (obj^.identifier) (tgt^.identifier) existing

-- | Game wrapper for the `flipExisting` function
gameFlipExisting :: GameExistingCollision id -> GameExistingCollision id
gameFlipExisting existing = GameExistingCollision
  (existing^.targetId)
  (existing^.objectId)
  (flipExisting $ existing^.existingCollision)

-- | Detects an existing collision between two physical objects.
-- |
-- | If no collision exists we return Nothing
collision :: PhysicalObject -> PhysicalObject -> Maybe ExistingCollision
collision obj1 obj2 = do
  penetration <- AABB.penetration (obj1^.shape) (obj2^.shape)
  pure $ ExistingCollision obj1 obj2 penetration

-- | Flips the collision to make it relative to `obj2` instead of `obj1`.
-- |
-- | In effect object 2 _becomes_ object 1 and vice versa
flipExisting :: ExistingCollision -> ExistingCollision
flipExisting existing = ExistingCollision
  (existing^.target)
  (existing^.object)
  (negate $ existing^.penetrationVector)
