{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE TemplateHaskell        #-}

module Arkmonoid.Physics.Shape.Types where

import Control.Lens
import Linear.V2

type Point = V2 Float
type Velocity = V2 Float
type Impulse = V2 Float
type Size = V2 Float
type FrameMovement = V2 Float

-- | An infinite line passing through `startPoint` and `endPoint`
data Line = Line
  { _startPoint :: Point
  , _endPoint :: Point
  } deriving Show
makeFieldsNoPrefix ''Line

-- | A bounded line start at `startPoint` and ending at `endPoint`
data Segment = Segment
  { _startPoint :: Point
  , _endPoint :: Point
  } deriving Show
makeFieldsNoPrefix ''Segment

data AABB = AABB
  { _minPoint :: Point -- ^ the smallest point of the AABB, i.e. the bottom left
  , _maxPoint :: Point -- ^ the largest point of the AABB, i.e. the top right
  } deriving (Show, Eq)
makeFieldsNoPrefix ''AABB
