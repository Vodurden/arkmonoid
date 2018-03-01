module Simulate (initializeWorld, step) where

import Data.Ecstasy
import Linear.V2
import Control.Monad (void)
import qualified Graphics.Gloss.Data.Color as G

import Types
import Simulate.Physics

initializeWorld :: GameSystem ()
initializeWorld = do
  void $ newEntity $ defEntity
    { position = Just (V2 50 100)
    , velocity = Just (V2 10 (-50))
    , geometry = Just $ Box 100 25
    , Types.color = Just G.red
    , bouncy = Just ()
    }

  void $ newEntity $ defEntity
    { position = Just (V2 0 0)
    , velocity = Just (V2 40 40)
    , geometry = Just $ Box 100 25
    , Types.color = Just G.green
    , bouncy = Just ()
    }

  void $ newEntity $ defEntity
    { position = Just (V2 10 10)
    , velocity = Just (V2 (-50) (30))
    , geometry = Just $ Box 100 25
    , Types.color = Just G.blue
    , bouncy = Just ()
    }

step :: Float -> GameSystem ()
step delta = stepPhysics delta
