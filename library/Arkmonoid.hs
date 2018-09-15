module Arkmonoid (main) where

import           Data.Ecstasy
import           Data.Functor.Identity (runIdentity)
import           Graphics.Gloss.Interface.Pure.Game

import Arkmonoid.Input
import Arkmonoid.Simulate
import Arkmonoid.Render

main :: IO ()
main =
    play display backgroundColor stepsPerSecond
      initialWorld render' handleInput' step'
  where stepsPerSecond = 120
        initialWorld = fst $ runIdentity $ yieldSystemT (0, defStorage) initializeWorld
        render' state = snd $ runIdentity $ yieldSystemT state render
        handleInput' event state = fst $ runIdentity $ yieldSystemT state (handleInput event)
        step' delta state = fst $ runIdentity $ yieldSystemT state (step delta)
