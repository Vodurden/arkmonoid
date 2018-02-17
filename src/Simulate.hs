module Simulate (initializeWorld, step) where

import Data.Ecstasy
import Linear.V2
import Control.Monad (void)
import qualified Graphics.Gloss.Data.Color as G

import Types

initializeWorld :: GameSystem ()
initializeWorld = do
  void $ newEntity $ defEntity
    { position = Just (V2 0 0)
    , geometry = Just $ Ball 50
    , Types.color = Just $ G.green
    }

  void $ newEntity $ defEntity
    { position = Just (V2 10 10)
    , velocity = Just (V2 5 5)
    , geometry = Just $ Rectangle 100 25
    , Types.color = Just $ G.blue
    }

step :: Float -> GameSystem ()
step delta = do
  stepVelocity delta

stepVelocity :: Float -> GameSystem ()
stepVelocity delta =
  emap $ do
    p <- get position
    v <- get velocity
    let scaledV = fmap (*delta) v
    pure defEntity'
      { position = Set $ (p + scaledV) }
