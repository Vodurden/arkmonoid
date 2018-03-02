module Render (display, backgroundColor, render) where

import Data.Ecstasy
import Linear.V2
import Graphics.Gloss.Interface.Pure.Game as G
import qualified Graphics.Gloss.Data.Color as G

import Types
import Extra.List
import qualified Simulate.Shape as S

-- | Configures the display window of the game
display :: G.Display
display = InWindow "hblock" (fromIntegral screenWidth, fromIntegral screenHeight) (0, 0)

-- | Background color of the game window
backgroundColor :: G.Color
backgroundColor = G.black

render :: GameSystem Picture
render = do
  gamePicture <- renderGame
  debugPicture <- renderDebug

  pure $ Pictures [gamePicture, debugPicture]

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

renderDebug :: GameSystem Picture
renderDebug = do
  -- Minkowski debugging
  candidates <- (efor . const) $ do
    with debug
    shape <- S.fromEntity
    pure shape
  let allCombinations = pairs candidates
  pure $ Pictures $ fmap (uncurry renderMinkowski) allCombinations

renderMinkowski :: S.Shape -> S.Shape -> Picture
renderMinkowski a b = G.Color debugColor $ G.Translate x y $ rectangleSolid w h
  where minkowski @ (S.AABB (V2 x y) (V2 w h)) = S.minkowskiDifference a b
        debugColor = if S.containsOrigin minkowski then G.red else G.white
