module Input (handleInput) where

import qualified Physics.Shape.AABB as AABB
import           Physics.Types
import           Types

import Control.Monad (guard)
import Control.Lens
import Data.Ecstasy
import Graphics.Gloss.Interface.Pure.Game
import Linear.V2
import Linear.Epsilon (nearZero)

handleInput :: Event -> GameSystem ()
handleInput event =
  unfreezeOnLeftClick event >>
  handleFollowMouse event

unfreezeOnLeftClick :: Event -> GameSystem ()
unfreezeOnLeftClick (EventKey (MouseButton LeftButton) _ _ _) =
  emap $ do
    with frozen
    pure defEntity' { frozen = Unset }
unfreezeOnLeftClick _ = pure ()

handleFollowMouse :: Event -> GameSystem ()
handleFollowMouse (EventMotion (xPos, yPos)) =
  emap $ do
    (FollowMouse followX followY) <- get followMouse
    phys <- get physicalObject
    let (V2 x y) = AABB.center (phys^.shape)
    let impulseX = if followX then xPos - x else 0
    let impulseY = if followY then yPos - y else 0
    let newImpulse = V2 impulseX impulseY

    guard (not $ nearZero newImpulse)

    let newPhysicalObject = phys & impulse .~ newImpulse
    pure $ defEntity'
      { physicalObject = Set newPhysicalObject }
handleFollowMouse _ = pure ()
