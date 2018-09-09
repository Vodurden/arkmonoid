{-# LANGUAGE DataKinds     #-}

module Arkmonoid.Simulate (initializeWorld, step) where

import Data.Ecstasy.Extended
import Data.Foldable
import Linear.V2
import Control.Monad
import qualified Graphics.Gloss.Data.Color as G

import           Arkmonoid.Types
import           Arkmonoid.Mortality.Types
import qualified Arkmonoid.Mortality.MortalitySystem as MortalitySystem
import           Arkmonoid.Physics.Types
import qualified Arkmonoid.Physics.Shape.AABB as AABB
import qualified Arkmonoid.Physics.PhysicsSystem as PhysicsSystem
import           Arkmonoid.Power.Types
import qualified Arkmonoid.Power.PowerSystem as PowerSystem

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
    MortalitySystem.step collisions
    PowerSystem.step collisions
    MortalitySystem.finalizeDead
  where
    linkEntIds = emap allEnts $ do
      eId <- queryEnt
      pure unchanged { entId = Set eId }

-- TODO: An actual level system.
paddle :: GameSystem ()
paddle = void $ createEntity $ newEntity
  { physicalObject = Just PhysicalObject
    { _velocity = V2 0 0
    , _impulse  = V2 0 0
    , _shape = AABB.mkAABB (V2 0 (-215)) (V2 100 15)
    , _material = Paddle
    , _frozen = True
    }

  , Arkmonoid.Types.color = Just G.red

  , followMouse = Just (FollowMouse True False)

  , powerReceiver = Just ()
  }

ball :: GameSystem ()
ball = void $ createEntity $ newEntity
  { physicalObject = Just PhysicalObject
    { _velocity = V2 250 (250)
    , _impulse = V2 0 0
    , _shape = AABB.mkAABB (V2 (-50) (-180)) (V2 10 10)
    , _material = Ball
    , _frozen = True
    }

  , Arkmonoid.Types.color = Just G.red

  , damage = Just $ DamageOnCollision 1
  }

block :: G.Color -> V2 Float -> GameSystem ()
block blockColor pos = void $ createEntity $ newEntity
  { physicalObject = Just PhysicalObject
    { _velocity = V2 0 0
    , _impulse = V2 0 0
    , _shape = AABB.mkAABB pos (V2 50 15)
    , _material = Solid
    , _frozen = True
    }

  , Arkmonoid.Types.color = Just blockColor
  , mortality = Just $ Mortal 1
  , powerSpawner = Just $ PowerSpawner Shorten
  }

blockLine :: G.Color -> V2 Float -> V2 Float -> Int -> GameSystem ()
blockLine blockColor start end blocks = traverse_ (block blockColor) positions
  where
    scale = (abs $ end - start) / (fromIntegral blocks)
    positions = fmap (\i -> start + scale * fromIntegral i) [0..blocks]
