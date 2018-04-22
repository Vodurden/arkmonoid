{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE TemplateHaskell        #-}

module Physics.CollisionResolution.Types where

import Physics.Types
import Physics.Shape.Types

import Control.Lens
import Linear.V2

data ExistingCollisionInfo = ExistingCollisionInfo
  { _penetrationVector :: V2 Float
  } deriving (Show, Eq)
makeFieldsNoPrefix ''ExistingCollisionInfo

data ImpendingCollisionInfo = ImpendingCollisionInfo
  { _objectLocation :: Point
  , _targetLocation :: Point
  , _segment :: Segment
  , _objectDistance :: Distance
  , _targetDistance :: Distance
  } deriving (Show, Eq)
makeFieldsNoPrefix ''ImpendingCollisionInfo

data CollisionInfo = ECollisionInfo ExistingCollisionInfo
                   | ICollisionInfo ImpendingCollisionInfo
                   deriving (Show, Eq)

data ResolvableCollision id = ResolvableCollision
  { _object :: PhysicalObject
  , _objectId :: id
  , _target :: Maybe PhysicalObject
  , _targetId :: Maybe id
  , _collisionInfo :: CollisionInfo
  } deriving (Show, Eq)
makeFieldsNoPrefix ''ResolvableCollision
