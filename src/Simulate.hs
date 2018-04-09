{-# LANGUAGE DataKinds     #-}

module Simulate (initializeWorld, step) where

import Data.Ecstasy
import Data.Foldable
import Linear.V2
import Control.Monad
import qualified Graphics.Gloss.Data.Color as G

import           Types
import           Extra.Ecstasy
import           Physics.Types
import qualified Physics.Shape.AABB as AABB
import qualified Physics.PhysicsSystem as PhysicsSystem
import qualified Damage.DamageSystem as DamageSystem

initializeWorld :: GameSystem ()
initializeWorld = do
  ball
  paddle
  blockLine G.red    (V2 (-290) 230) (V2 290 230) 10
  blockLine G.orange (V2 (-290) 210) (V2 290 210) 10
  blockLine G.yellow (V2 (-290) 190) (V2 290 190) 10
  blockLine G.green  (V2 (-290) 170) (V2 290 170) 10
  blockLine G.blue   (V2 (-290) 150) (V2 290 150) 10

step :: Float -> GameSystem ()
step delta = do
    linkEntIds
    collisions <- PhysicsSystem.step delta
    DamageSystem.step collisions
  where
    linkEntIds = emapIndexed $ \ent -> do
      without entId
      pure defEntity' { entId = Set ent }

-- TODO: An actual level system.
paddle :: GameSystem ()
paddle = void $ newEntity $ defEntity
  { physicalObject = Just PhysicalObject
    { _velocity = V2 0 0
    , _impulse  = V2 0 0
    , _shape = AABB.mkAABB (V2 0 (-215)) (V2 100 15)
    , _material = Paddle
    , _frozen = True
    }

  , Types.color = Just G.red

  , followMouse = Just (FollowMouse True False)
  }

ball :: GameSystem ()
ball = void $ newEntity $ defEntity
  { physicalObject = Just PhysicalObject
    { _velocity = V2 250 (250)
    , _impulse = V2 0 0
    , _shape = AABB.mkAABB (V2 (-50) (-180)) (V2 10 10)
    , _material = Ball
    , _frozen = True
    }

  , Types.color = Just G.red

  , damage = Just 1
  }

block :: G.Color -> V2 Float -> GameSystem ()
block color pos = void $ newEntity $ defEntity
  { physicalObject = Just PhysicalObject
    { _velocity = V2 0 0
    , _impulse = V2 0 0
    , _shape = AABB.mkAABB pos (V2 50 15)
    , _material = Solid
    , _frozen = True
    }

  , Types.color = Just color
  , health = Just 1
  }

blockLine :: G.Color -> V2 Float -> V2 Float -> Int -> GameSystem ()
blockLine color start end blocks = traverse_ (block color) positions
  where
    scale = (abs $ end - start) / (fromIntegral blocks)
    positions = fmap (\i -> start + scale * fromIntegral i) [0..blocks]
