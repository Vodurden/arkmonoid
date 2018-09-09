module Arkmonoid.Mortality.MortalitySystem where

import Control.Monad
import Data.Ecstasy.Extended
import Data.Foldable
import Data.Maybe

import           Arkmonoid.Types
import           Arkmonoid.Mortality.Types
import qualified Arkmonoid.Mortality.Mortality as Mortality
import           Arkmonoid.Physics.CollisionDetection.Types
import qualified Arkmonoid.Physics.CollisionDetection.GameCollisions as GameCollisions

step :: GameCollisions Ent -> GameSystem ()
step = damageCollidingEntities

-- | Removes dead entities from the entity pool
finalizeDead :: GameSystem ()
finalizeDead = void $ emap allEnts $ do
  Dead <- query mortality
  pure delEntity

damageCollidingEntities :: GameCollisions Ent -> GameSystem ()
damageCollidingEntities collisions =
    let collidingEnts = GameCollisions.collidingIds collisions
    in traverse_ (uncurry damageFromTo) collidingEnts
  where
    -- | Apply damage from the damager to the damagee
    damageFromTo :: Ent -> Ent -> GameSystem ()
    damageFromTo damager damagee = do
      -- Entities are Harmless by default
      maybeDamage <- eget damager (query damage)
      let dmg = fromMaybe Harmless maybeDamage

      -- Entities are Immortal by default
      maybeMortal <- eget damagee (query mortality)
      let mortal = fromMaybe Immortal maybeMortal

      let newMortal = Mortality.damage dmg mortal

      forEnt damagee $ do
        guard (newMortal /= mortal)
        pure $ unchanged { mortality = Set newMortal }
