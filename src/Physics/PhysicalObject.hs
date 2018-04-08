module Physics.PhysicalObject where

import Physics.Types

import Control.Lens
import Linear.V2

-- | Calculates how far this object will move this frame if no collision occurs
movement :: Float -> PhysicalObject -> V2 Float
movement delta obj = (fmap (*delta) (obj^.velocity)) + obj^.impulse
