{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Physics.CollisionResolution where

import           Physics.Types
import           Physics.CollisionDetection.Types
import qualified Physics.GameObject as GameObject
import qualified Physics.Shape.Segment as Segment

import Control.Lens
import Extra.V2 (reflect)
import Linear.V2

-- | Resolves the collisions on an object
resolveCollision :: GameCollision id -> GameObject id -> GameObject id
resolveCollision collision =
  resolveVelocity collision
  . resolvePosition collision

-- | Updates the objects position such that it is no longer colliding
resolvePosition :: GameCollision id -> GameObject id -> GameObject id
resolvePosition collision obj = case collision of
    (GBoundaryCollision _ collisions) -> foldr resolvePositionBoundary obj collisions
    (GExistingCollision _ collisions) -> foldr resolvePositionExisting obj collisions
    (GImpendingCollision _ impending) -> resolvePositionImpending impending obj
  where
    resolvePositionBoundary :: GameBoundaryCollision id -> GameObject id -> GameObject id
    resolvePositionBoundary collision obj =
      let penVector = collision^.boundaryCollision.penetrationVector
      in GameObject.moveObjectByAmount penVector obj

    resolvePositionExisting :: GameExistingCollision id -> GameObject id -> GameObject id
    resolvePositionExisting collision obj =
      let penVector = collision^.existingCollision.penetrationVector
      in GameObject.moveObjectByAmount penVector obj

    resolvePositionImpending :: GameImpendingCollision id -> GameObject id -> GameObject id
    resolvePositionImpending collision obj =
      let collisionPoint = collision^.impendingCollision.objectLocation
      in GameObject.moveObjectTo collisionPoint obj

-- | Updates the objects velocity as a result of the collision.
resolveVelocity :: forall id. GameCollision id -> GameObject id -> GameObject id
resolveVelocity = bounce
  where
    -- | Bounces the object if it should be bounced
    bounce :: GameCollision id -> GameObject id -> GameObject id
    bounce collision obj | obj^.physical.material == Ball =
      let v = obj^.physical.velocity
          newV = reflectByCollision collision v
      in set (physical.velocity) newV obj
    bounce _ obj = obj

    reflectByCollision :: GameCollision id -> V2 Float -> V2 Float
    reflectByCollision (GBoundaryCollision _ collisions) vel =
      foldr (\b v -> reflect v (b^.boundaryCollision.penetrationVector)) vel collisions
    reflectByCollision (GExistingCollision _ collisions) vel =
      foldr (\b v -> reflect v (b^.existingCollision.penetrationVector)) vel collisions
    reflectByCollision (GImpendingCollision _ imp) vel =
      let reflectVector = Segment.normalVector (imp^.impendingCollision.segment)
      in reflect vel reflectVector
