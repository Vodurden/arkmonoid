{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arkmonoid (main) where

import           Control.Monad.Random
import           Data.Ecstasy
import           Graphics.Gloss.Interface.IO.Game

import Arkmonoid.Types
import Arkmonoid.Input
import Arkmonoid.Simulate
import Arkmonoid.Render

yieldArkmonad :: forall a.
                 GameState
              -> Arkmonad a
              -> IO (GameState, a)
yieldArkmonad state @ (GameState { _gameState = gameState, _random = rnd }) (Arkmonad system) =
  let
    next' :: Underlying (GameSystemState Underlying, a)
    next' = yieldSystemT gameState system

    next'' :: IO ((GameSystemState Underlying, a), StdGen)
    next'' = runRandT next' rnd
  in do
    ((nextGameState, value), nextRnd) <- next''
    let nextState = state { _gameState = nextGameState, _random = nextRnd }
    pure $ (nextState, value)

render' :: GameState -> IO Picture
render' state = snd <$> yieldArkmonad state (Arkmonad render)

handleInput' :: Event -> GameState -> IO GameState
handleInput' event state = fst <$> yieldArkmonad state (Arkmonad $ handleInput event)

step' :: Float -> GameState -> IO GameState
step' delta state = fst <$> yieldArkmonad state (Arkmonad $ step delta)

main :: IO ()
main = do
  rnd <- newStdGen
  let initialState = GameState { _gameState = (0, defStorage) , _random = rnd}
  initialWorld <- fst <$> yieldArkmonad initialState (Arkmonad initializeWorld)

  let stepsPerSecond = 120
  playIO display backgroundColor stepsPerSecond initialWorld render' handleInput' step'
