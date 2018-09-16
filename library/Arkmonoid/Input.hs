module Arkmonoid.Input (handleInput) where

import qualified Arkmonoid.Physics.Shape.AABB as AABB
import           Arkmonoid.Physics.Types
import           Arkmonoid.Types

import Control.Monad (guard)
import Control.Lens
import Data.Ecstasy
import Graphics.Gloss.Interface.Pure.Game
import Linear.V2
import Linear.Epsilon (nearZero)

handleInput :: (Monad m) => Event -> GameSystemT m ()
handleInput event =
  unfreezeOnLeftClick event >>
  handleFollowMouse event

unfreezeOnLeftClick :: (Monad m) => Event -> GameSystemT m ()
unfreezeOnLeftClick (EventKey (MouseButton LeftButton) _ _ _) =
  emap allEnts $ do
    phys <- query physicalObject

    -- Don't update if we're already unfrozen.
    guard (phys^.frozen /= False)

    pure unchanged { physicalObject = Set (set frozen False phys) }
unfreezeOnLeftClick _ = pure ()

handleFollowMouse :: (Monad m) => Event -> GameSystemT m ()
handleFollowMouse (EventMotion (xPos, yPos)) =
  emap allEnts $ do
    (FollowMouse followX followY) <- query followMouse
    phys <- query physicalObject
    let (V2 x y) = AABB.center (phys^.shape)
    let impulseX = if followX then xPos - x else 0
    let impulseY = if followY then yPos - y else 0
    let newImpulse = V2 impulseX impulseY

    guard (not $ nearZero newImpulse)

    let newPhysicalObject = phys & impulse .~ newImpulse
    pure $ unchanged
      { physicalObject = Set newPhysicalObject }
handleFollowMouse _ = pure ()
