{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Control.Monad.Trans.State as S

import Data.Ecstasy
import Graphics.Gloss.Interface.Pure.Game

import Types
import Input
import Simulate
import Render

playGame :: IO ()
playGame = play display backgroundColor framesPerSecond initialWorld render' handleInput' step'
  where framesPerSecond = 60
        initialWorld = S.execState initializeWorld (0, defWorld)
        render' = S.evalState render
        handleInput' event = S.execState (handleInput event)
        step' delta = S.execState (step delta)

main :: IO ()
main = playGame
