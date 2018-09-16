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

step :: (Monad m) => GameCollisions Ent -> GameSystemT m ()
step = damageCollidingEntities

-- | Removes dead entities from the entity pool
finalizeDead :: (Monad m) => GameSystemT m ()
finalizeDead = void $ emap allEnts $ do
  m <- query mortality
  guard (m == Dead)
  pure delEntity

damageCollidingEntities :: (Monad m) => GameCollisions Ent -> GameSystemT m ()
damageCollidingEntities collisions =
    let collidingEnts = GameCollisions.collidingIds collisions
    in traverse_ (uncurry damageFromTo) collidingEnts
  where
    -- | Apply damage from the damager to the damagee
    damageFromTo :: (Monad m) => Ent -> Ent -> GameSystemT m ()
    damageFromTo damager damagee = do
      -- Entities are Harmless by default
      maybeDamage <- runQueryT damager (query damage)
      let dmg = fromMaybe Harmless maybeDamage

      emap (anEnt damagee) $ do
        -- Entities are Immortal by default
        mortal <- queryDef Immortal mortality
        let newMortal = Mortality.damage dmg mortal
        pure $ unchanged { mortality = Set newMortal }
