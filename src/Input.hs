module Input (handleInput) where

import Data.Ecstasy
import Linear.V2
import Graphics.Gloss.Interface.Pure.Game

import Types

handleInput :: Event -> GameSystem ()
handleInput event =
  unfreezeOnLeftClick event >>
  handleFollowMouseX event

unfreezeOnLeftClick :: Event -> GameSystem ()
unfreezeOnLeftClick (EventKey (MouseButton LeftButton) _ _ _) =
  emap $ pure defEntity' { frozen = Unset }
unfreezeOnLeftClick _ = pure ()

handleFollowMouseX :: Event -> GameSystem ()
handleFollowMouseX (EventMotion (xPos, _)) =
  emap $ do
    with followMouseX
    (V2 x _) <- get position
    let newImpulse = V2 (xPos - x) 0
    pure defEntity'
      { impulse = Set newImpulse }
handleFollowMouseX _ = pure ()
