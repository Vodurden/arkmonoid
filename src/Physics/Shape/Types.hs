{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE TemplateHaskell        #-}

module Physics.Shape.Types where

import Control.Lens
import Data.Ecstasy
import Linear.V2

type Point = V2 Float
type Size = V2 Float
type FrameMovement = V2 Float

-- | An infinite line passing through `startPoint` and `endPoint`
data Line = Line
  { _startPoint :: Point
  , _endPoint :: Point
  } deriving Show

-- | A bounded line start at `startPoint` and ending at `endPoint`
data Segment = Segment
  { _startPoint :: Point
  , _endPoint :: Point
  } deriving Show

data AABB = AABB
  { _minPoint :: Point -- ^ the smallest point of the AABB, i.e. the bottom left
  , _maxPoint :: Point -- ^ the largest point of the AABB, i.e. the top right
  } deriving (Show)

makeFieldsNoPrefix ''Line
makeFieldsNoPrefix ''Segment
makeFieldsNoPrefix ''AABB

data CollisionModel = DynamicAABB AABB FrameMovement
                    | StaticAABB AABB
  deriving Show

data BoundarySide = BoundLeft | BoundRight
  deriving Show

data Boundary = Boundary Line BoundarySide
  deriving Show

-- | Represents the type of collison that is occurring
-- |
-- | PenetrationCollision: A collison based on the depth of overlap. Usually occurs if the objects
-- |                       are already colliding. Includes a penetration vector that will
-- |                       resolve this collision.
-- |
-- | PointCollision: A collision that will occur at a given point. Usually occurs if the objects
-- |                 are not colliding but will collide this frame. Includes the point of collision
-- |                 and a segment representing the surface on which the collision occured.
data CollisionType = PenetrationCollision PenetrationVector
                   | PointCollision Point Segment Float
  deriving Show

-- | Represents a collision from the perspective of a single shape.
data Collision = EntCollision Ent CollisionType
               | BoundaryCollision CollisionType
  deriving Show

-- | Represents the depth of penetration between two colliding shapes.
-- |
-- | When added to the position of the colliding shape this will resolve
-- | the collision
type PenetrationVector = (V2 Float)
