module Arkmonoid.Power.PowerSystem where

import           Control.Monad
import           Control.Lens
import           Data.Ecstasy.Extended
import           Data.Foldable
import qualified Graphics.Gloss.Data.Color as G
import           Linear.V2

import           Arkmonoid.Types
import           Arkmonoid.Mortality.Types
import           Arkmonoid.Power.Types
import           Arkmonoid.Physics.Types
import           Arkmonoid.Physics.Shape.Types
import qualified Arkmonoid.Physics.Shape.AABB as AABB

step :: GameSystem ()
step = spawnPowers

spawnPowers :: GameSystem ()
spawnPowers = do
    powerInfos <- (efor . const) $ do
      Dead <- get mortality
      (PowerSpawner power) <- get powerSpawner
      physics <- get physicalObject

      pure (AABB.center $ physics^.shape, power)

    traverse_ (uncurry spawnPower) powerInfos
  where
    spawnPower :: Point -> Power -> GameSystem ()
    spawnPower point power = void $ newEntity $ defEntity
      { powerApplier = Just (PowerApplier power)
      , physicalObject = Just $ powerUpPhysics point
      , Arkmonoid.Types.color = Just G.blue
      }

    powerUpPhysics :: Point -> PhysicalObject
    powerUpPhysics pos = PhysicalObject
      { _velocity = V2 0 (-150)
      , _impulse = V2 0 0
      , _shape = AABB.mkAABB pos (V2 20 20)
      , _material = Solid
      , _frozen = False
      }
