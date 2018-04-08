module Render (display, backgroundColor, render) where

import           Extra.List
import           Physics.Types
import           Physics.Shape.Types
import qualified Physics.Shape.AABB as AABB
import           Types

import           Control.Lens
import           Data.Ecstasy
import qualified Graphics.Gloss.Data.Color as G
import           Graphics.Gloss.Interface.Pure.Game as G
import           Linear.V2

-- | Configures the display window of the game
display :: G.Display
display = InWindow "arkmonoid" (fromIntegral screenWidth, fromIntegral screenHeight) (0, 0)

-- | Background color of the game window
backgroundColor :: G.Color
backgroundColor = G.black

render :: GameSystem Picture
render = do
  gamePicture <- renderGame

  pure $ Pictures [gamePicture]

renderGame :: GameSystem Picture
renderGame = do
    pics <- (efor . const) entPicture
    pure $ Pictures pics

entPicture :: (Monad m) => GameQueryT m G.Picture
entPicture = do
  obj <- get physicalObject
  let physicsShape = obj^.shape
  let (V2 x y) = AABB.center physicsShape
  let (V2 w h) = AABB.size physicsShape

  color <- getMaybe Types.color
  let colorFn = maybe id G.Color color

  pure $ colorFn $ G.Translate x y $ rectangleSolid w h
