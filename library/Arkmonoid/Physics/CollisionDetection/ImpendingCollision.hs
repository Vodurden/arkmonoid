module Arkmonoid.Physics.CollisionDetection.ImpendingCollision where

import           Control.Lens
import           Control.Monad (guard)
import           Linear.V2
import           Linear.Epsilon (nearZero)

import           Arkmonoid.Physics.Types
import           Arkmonoid.Physics.CollisionDetection.Types
import qualified Arkmonoid.Physics.PhysicalObject as PhysicalObject
import           Arkmonoid.Physics.Shape.Types
import qualified Arkmonoid.Physics.Shape.AABB as AABB
import qualified Arkmonoid.Physics.Shape.Segment as Segment
import qualified Arkmonoid.Physics.CollisionDetection.Raytrace as Raytrace

-- | Detects collisions between two shapes that could collide this frame.
-- |
-- | Assumes that the shapes are _not_ already colliding
collision :: Float -> PhysicalObject -> PhysicalObject -> Maybe ImpendingCollision
collision delta obj tgt = do
  let objMovement = PhysicalObject.movement delta obj
  let tgtMovement = PhysicalObject.movement delta tgt
  let relativeMotion = objMovement - tgtMovement

  -- No need to check for impending collisions if neither object is moving
  guard (not $ nearZero relativeMotion)

  let minkowskiAABB = AABB.minkowskiDifference (tgt^.shape) (obj^.shape)
  let ray = Segment (V2 0 0) relativeMotion
  trace <- Raytrace.traceAABB ray minkowskiAABB
  minkowskiIntersectionFraction <- Segment.intersectionFraction ray (trace^.point)

  let objAllowedMovement = fmap (*minkowskiIntersectionFraction) objMovement
  let tgtAllowedMovement = fmap (*minkowskiIntersectionFraction) tgtMovement

  -- This is an ugly hack to stop velocity-based collision from getting stuck on various
  -- blocks.
  let slightlyShorterObjMovement = fmap (*0.98) objAllowedMovement
  let slightlyShorterTgtMovement = fmap (*0.98) tgtAllowedMovement

  let objLocation = AABB.center (obj^.shape) + slightlyShorterObjMovement
  let tgtLocation = AABB.center (tgt^.shape) + slightlyShorterTgtMovement

  pure $ ImpendingCollision
    obj tgt
    objLocation tgtLocation
    (trace^.segment)
    (trace^.distance) (trace^.remainingDistance)

-- | Flips the collision to make it relative to `obj2` instead of `obj1`.
-- |
-- | In effect object 2 _becomes_ object 1 and vice versa
flipImpending :: ImpendingCollision -> ImpendingCollision
flipImpending impending = ImpendingCollision
  (impending^.target)
  (impending^.object)
  (impending^.targetLocation)
  (impending^.objectLocation)
  (impending^.segment)
  (impending^.targetDistance)
  (impending^.objectDistance)

-- | Game wrapper for the `collision` function
gameCollision :: Float -> GameObject id -> GameObject id -> Maybe (GameImpendingCollision id)
gameCollision delta obj1 obj2 = do
  impending <- collision delta (obj1^.physical) (obj2^.physical)
  pure $ GameImpendingCollision (obj1^.identifier) (obj2^.identifier) impending

-- | Game wrapper for the `flipImpending` function
gameFlipImpending :: GameImpendingCollision id -> GameImpendingCollision id
gameFlipImpending impending = GameImpendingCollision
  (impending^.targetId)
  (impending^.objectId)
  (flipImpending $ impending^.impendingCollision)
