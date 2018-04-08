module Physics.GameObject where

import Physics.Types
import Physics.Shape.Types
import qualified Physics.Shape.AABB as AABB
import qualified Physics.PhysicalObject as PhysicalObject

import Control.Lens
import Linear.V2

moveObject :: Float -> GameObject id -> GameObject id
moveObject delta obj = moveObjectByAmount (PhysicalObject.movement delta (obj^.physical)) obj

moveObjectTo :: Point -> GameObject id -> GameObject id
moveObjectTo p = over (physical.shape) (AABB.moveTo p)

moveObjectByAmount :: V2 Float -> GameObject id -> GameObject id
moveObjectByAmount amount = over (physical.shape) (AABB.moveBy amount)
