module Arkmonoid (main) where

import qualified Control.Monad.Trans.State as S
import           Data.Ecstasy
import           Graphics.Gloss.Interface.Pure.Game

import Arkmonoid.Input
import Arkmonoid.Simulate
import Arkmonoid.Render

main :: IO ()
main = play display backgroundColor stepsPerSecond initialWorld render' handleInput' step'
  where stepsPerSecond = 120
        initialWorld = S.execState initializeWorld (0, defWorld)
        render' = S.evalState render
        handleInput' event = S.execState (handleInput event)
        step' delta = S.execState (step delta)
