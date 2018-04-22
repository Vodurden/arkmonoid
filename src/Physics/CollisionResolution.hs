{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Physics.CollisionResolution where

import           Physics.Types
import           Physics.CollisionDetection.Types
import qualified Physics.CollisionDetection.GameCollision as GameCollision
import qualified Physics.GameObject as GameObject
import qualified Physics.Shape.Segment as Segment

import Control.Lens
import Extra.V2 (reflect)
import Linear.V2

type CollisionResolver id = GameCollision id -> GameObject id -> GameObject id
type CollisionMatcher id = GameCollision id -> GameObject id -> Bool

data PointCollision = PointCollision

-- | Resolves the collisions on an object
resolveCollision :: CollisionResolver id
resolveCollision collision =
  resolveVelocity collision
  . resolvePosition collision

-- | Updates the objects position such that it is no longer colliding
resolvePosition :: CollisionResolver id
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
resolveVelocity :: forall id. CollisionResolver id
resolveVelocity c =
    onMaterialCombo Ball Paddle forceBounceUp c
    . onMaterial Ball bounce c
  where
    -- | Triggers the given function when the object/collision material requirements are met.
    onMaterialCombo :: Material -> Material -> (CollisionResolver id) -> CollisionResolver id
    onMaterialCombo objMaterial collisionMaterial f collision obj =
        if hasMaterialCombo
        then f collision obj
        else obj
      where
        hasMaterialCombo = objHasMaterial && collisionHasMaterial
        objHasMaterial = obj^.physical.material == objMaterial
        collisionHasMaterial = GameCollision.hasMaterial collision collisionMaterial

    onMaterial :: Material -> (CollisionResolver id) -> CollisionResolver id
    onMaterial objMaterial f collision obj =
      if obj^.physical.material == objMaterial
      then f collision obj
      else obj

    nearXEdge :: Float -> (CollisionResolver id) -> CollisionResolver id
    nearXEdge pct resolver collision obj = undefined

    -- | Force the object to invert it's X velocity
    forceReverseX :: GameCollision id -> GameObject id -> GameObject id
    forceReverseX _ = over (physical.velocity._x) negate

    -- | Force the object to bounce up
    forceBounceUp :: GameCollision id -> GameObject id -> GameObject id
    forceBounceUp _ = over (physical.velocity._y) abs

    -- | Bounces the object
    bounce :: GameCollision id -> GameObject id -> GameObject id
    bounce collision obj =
        let v = obj^.physical.velocity
            newV = reflectByCollision collision v
        in set (physical.velocity) newV obj
      where
        reflectByCollision :: GameCollision id -> V2 Float -> V2 Float
        reflectByCollision (GBoundaryCollision _ collisions) vel =
          foldr (\b v -> reflect v (b^.boundaryCollision.penetrationVector)) vel collisions
        reflectByCollision (GExistingCollision _ collisions) vel =
          foldr (\b v -> reflect v (b^.existingCollision.penetrationVector)) vel collisions
        reflectByCollision (GImpendingCollision _ imp) vel =
          let reflectVector = Segment.normalVector (imp^.impendingCollision.segment)
          in reflect vel reflectVector
