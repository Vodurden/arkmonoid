module Render (display, backgroundColor, render) where

import Data.Ecstasy
import Linear.V2
import Graphics.Gloss.Interface.Pure.Game as G
import qualified Graphics.Gloss.Data.Color as G

import Types
import Extra.List

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
    pictures <- (efor . const) $ do
      (V2 x y) <- get position
      geometry <- get geometry
      color <- getMaybe Types.color
      let colorFn = maybe id G.Color color
      pure $ colorFn $ G.Translate x y $ geometryPicture geometry

    pure $ Pictures pictures

-- | Construct a picture given some geometry
geometryPicture :: Geometry -> G.Picture
geometryPicture (Box w h) = rectangleSolid w h
