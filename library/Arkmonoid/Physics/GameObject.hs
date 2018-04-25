module Arkmonoid.Physics.GameObject where

import Control.Lens
import Linear.V2

import           Arkmonoid.Physics.Types
import           Arkmonoid.Physics.Shape.Types
import qualified Arkmonoid.Physics.Shape.AABB as AABB
import qualified Arkmonoid.Physics.PhysicalObject as PhysicalObject

moveObject :: Float -> GameObject id -> GameObject id
moveObject delta obj = moveObjectByAmount (PhysicalObject.movement delta (obj^.physical)) obj

moveObjectTo :: Point -> GameObject id -> GameObject id
moveObjectTo p = over (physical.shape) (AABB.moveTo p)

moveObjectByAmount :: V2 Float -> GameObject id -> GameObject id
moveObjectByAmount amount = over (physical.shape) (AABB.moveBy amount)
