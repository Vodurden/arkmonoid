{-# LANGUAGE DuplicateRecordFields  #-}

module Physics.CollisionResolution.ResolvableCollision where

import           Physics.CollisionDetection.Types
import           Physics.CollisionResolution.Types (ResolvableCollision)
import qualified Physics.CollisionResolution.Types as CR

import Control.Lens

fromGameCollision :: GameCollision id -> [ResolvableCollision id]
fromGameCollision (GBoundaryCollision _ cs) = fmap fromGameBoundaryCollision cs
fromGameCollision (GExistingCollision _ cs) = fmap fromGameExistingCollision cs
fromGameCollision (GImpendingCollision _ c) = [fromGameImpendingCollision c]

fromGameBoundaryCollision :: GameBoundaryCollision id -> ResolvableCollision id
fromGameBoundaryCollision c = CR.ResolvableCollision
  { _object = c^.boundaryCollision.object
  , _objectId = c^.objectId
  , _target = Nothing
  , _targetId = Nothing
  , _collisionInfo = CR.ECollisionInfo CR.ExistingCollisionInfo
    { _penetrationVector = c^.boundaryCollision.penetrationVector }
  }

fromGameExistingCollision :: GameExistingCollision id -> ResolvableCollision id
fromGameExistingCollision c = CR.ResolvableCollision
  { _object = c^.existingCollision.object
  , _objectId = c^.objectId
  , _target = Just $ c^.existingCollision.target
  , _targetId = Just $ c^.targetId
  , _collisionInfo = CR.ECollisionInfo CR.ExistingCollisionInfo
    { _penetrationVector = c^.existingCollision.penetrationVector }
  }

fromGameImpendingCollision :: GameImpendingCollision id -> ResolvableCollision id
fromGameImpendingCollision c = CR.ResolvableCollision
  { _object = c^.impendingCollision.object
  , _objectId = c^.objectId
  , _target = Just $ c^.impendingCollision.target
  , _targetId = Just $ c^.targetId
  , _collisionInfo = CR.ICollisionInfo CR.ImpendingCollisionInfo
    { _objectLocation = c^.impendingCollision.objectLocation
    , _targetLocation = c^.impendingCollision.targetLocation
    , _segment  = c^.impendingCollision.segment
    , _objectDistance = c^.impendingCollision.objectDistance
    , _targetDistance = c^.impendingCollision.targetDistance
    }
  }
