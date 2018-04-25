module Arkmonoid.Physics.PhysicalObject where

import Control.Lens
import Linear.V2

import Arkmonoid.Physics.Types

-- | Calculates how far this object will move this frame if no collision occurs
movement :: Float -> PhysicalObject -> V2 Float
movement delta obj = (fmap (*delta) (obj^.velocity)) + obj^.impulse
