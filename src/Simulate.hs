{-# LANGUAGE DataKinds     #-}

module Simulate (initializeWorld, step) where

import Data.Ecstasy
import Data.Foldable
import Linear.V2
import Control.Monad
import qualified Graphics.Gloss.Data.Color as G

import Types
import qualified Simulate.Physics as Physics
import qualified Simulate.Damage as Damage

initializeWorld :: GameSystem ()
initializeWorld = do
  ball
  paddle
  blockLine (V2 (-290) 310) (V2 290 310) 11

step :: Float -> GameSystem ()
step delta = do
    physicsStep delta
    Damage.step
  where
    physicsStep = Physics.step onEntityCollision

    onEntityCollision :: Ent -> Ent -> GameSystem ()
    onEntityCollision = Damage.onEntityCollision

-- TODO: An actual level system.
paddle :: GameSystem ()
paddle = void $ newEntity $ defEntity
  { position = Just (V2 0 (-220))
  , geometry = Just $ Box 100 10
  , Types.color = Just G.green

  , frozen = Just ()
  , followMouse = Just (FollowMouse True False)
  , debug = Just ()
  }

ball :: GameSystem ()
ball = void $ newEntity $ defEntity
  { position = Just (V2 (-50) (-180))
  , velocity = Just (V2 250 (-250))
  , geometry = Just $ Box 10 10
  , Types.color = Just G.red

  , frozen = Just ()
  , bouncy = Just ()
  , debug = Just ()

  , damage = Just 1
  }

block :: V2 Float -> GameSystem ()
block pos = void $ newEntity $ defEntity
  { position = Just pos
  , geometry = Just $ Box 50 10
  , Types.color = Just G.blue
  , health = Just 1
  }

blockLine :: V2 Float -> V2 Float -> Int -> GameSystem ()
blockLine start end blocks = traverse_ block positions
  where
    scale = (abs $ end - start) / (fromIntegral blocks)
    positions = fmap (\i -> start + scale * fromIntegral i) [1..blocks]
