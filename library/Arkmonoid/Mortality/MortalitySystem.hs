module Arkmonoid.Mortality.MortalitySystem where

import Control.Monad
import Data.Ecstasy
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
  m <- query mortality
  guard (m == Dead)
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
      maybeDamage <- runQueryT damager (query damage)
      let dmg = fromMaybe Harmless maybeDamage

      emap (anEnt damagee) $ do
        -- Entities are Immortal by default
        mortal <- queryDef Immortal mortality
        let newMortal = Mortality.damage dmg mortal
        pure $ unchanged { mortality = Set newMortal }
