{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE TemplateHaskell        #-}

module Physics.Types where

import Physics.Shape.Types

import Control.Lens

-- | Represents the material the object is made from. This can be used
-- | to affect the behavior of the object when a collision occurs.
-- |
-- | Some material combinations may also have special interaction rules.
data Material = Solid | Ball | Paddle
  deriving (Show, Eq, Ord)

-- | Represents a boundary of the world. We assume that nothing is allowed to move past
-- | this boundary.
-- |
-- | The boundary side is relative to the direction of the line. I.e. it is relative to
-- | the vector from the start point of the line to the end point of the line
data Boundary = LeftBoundary  Line Material
              | RightBoundary Line Material
  deriving Show

-- | An object to be physicall simulated by the physics system
data PhysicalObject = PhysicalObject
  { _velocity :: Velocity
  , _impulse  :: Impulse
  , _shape    :: AABB
  , _material :: Material
  } deriving (Show, Eq)
makeFieldsNoPrefix ''PhysicalObject

data GameObject id = GameObject
  { _identifier :: id
  , _physical   :: PhysicalObject
  } deriving (Show, Eq)
makeFieldsNoPrefix ''GameObject
