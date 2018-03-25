module Input (handleInput) where

import Data.Ecstasy
import Linear.V2
import Graphics.Gloss.Interface.Pure.Game

import Types

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
    (V2 x y) <- get position
    let impulseX = if followX then xPos - x else 0
    let impulseY = if followY then yPos - y else 0
    let newImpulse = V2 impulseX impulseY
    pure defEntity'
      { impulse = Set newImpulse }
handleFollowMouse _ = pure ()
