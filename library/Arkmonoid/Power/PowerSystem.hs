{-# LANGUAGE DataKinds    #-}

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
import           Arkmonoid.Physics.CollisionDetection.Types
import qualified Arkmonoid.Physics.CollisionDetection.GameCollisions as GameCollisions

step :: GameCollisions Ent -> GameSystem ()
step collisions = do
  applyPowers collisions
  spawnPowers

applyPowers :: GameCollisions Ent -> GameSystem ()
applyPowers collisions =
    let collidingEnts = GameCollisions.collidingIds collisions
    in traverse_ (uncurry applyPowerFromTo) collidingEnts

applyPowerFromTo :: Ent -> Ent -> GameSystem ()
applyPowerFromTo powerer poweree = do
  maybePowerApplication <- eget powerer (query powerApplier)
  canReceive <- eget poweree (query powerReceiver)

  case (maybePowerApplication, canReceive) of
    (Just powerApplication, Just ()) -> do
      forEnt poweree $ applyPowerTo powerApplication
      forEnt powerer $ pure $ unchanged { mortality = Set Dead }
    (Nothing, _) -> pure ()
    (_, Nothing) -> pure ()

applyPowerTo :: (Monad m)
             => PowerApplier
             -> GameQueryT m (Entity' 'SetterOf)
applyPowerTo applier = do
    with powerReceiver
    case applier of
      (PowerApplier Widen) -> applySizeChange 1.1
      (PowerApplier Shorten) -> applySizeChange 0.9
  where
    applySizeChange :: (Monad m)
                    => Float
                    -> GameQueryT m (Entity' 'SetterOf)
    applySizeChange percentage = do
      physics <- query physicalObject
      let change = AABB.clampWidth 50 200 . AABB.scaleWidth percentage
      let newShape = over shape change physics
      pure $ unchanged { physicalObject = Set newShape }

spawnPowers :: GameSystem ()
spawnPowers = do
    powerInfos <- efor allEnts $ do
      Dead <- query mortality
      (PowerSpawner power) <- query powerSpawner
      physics <- query physicalObject

      pure (AABB.center $ physics^.shape, power)

    traverse_ (uncurry spawnPower) powerInfos
  where
    spawnPower :: Point -> Power -> GameSystem ()
    spawnPower pos power = void $ createEntity $ newEntity
      { powerApplier = Just (PowerApplier power)
      , physicalObject = Just $ powerUpPhysics pos
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
