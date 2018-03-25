module Damage.DamageSystem where

import Control.Monad
import Data.Foldable
import Data.Ecstasy
import Extra.Ecstasy
import Data.Maybe
import qualified Data.Map.Strict as Map

import Types
import Physics.Shape.Types
import qualified Physics.System as PhysicsSystem

step :: Map.Map Ent [Collision] -> GameSystem ()
step collisions = do
  damageCollidingEntities collisions
  killDeadEntities

damageCollidingEntities :: Map.Map Ent [Collision] -> GameSystem ()
damageCollidingEntities collisions =
    let collidingEnts = PhysicsSystem.collidingEnts collisions
    in traverse_ (uncurry damageFromTo) collidingEnts
  where
    -- | Apply damage from the damager to the damagee
    damageFromTo :: Ent -> Ent -> GameSystem ()
    damageFromTo damager damagee = do
      maybeDamage <- eget damager (get damage)
      let dmg = fromMaybe 0 maybeDamage

      doDamageTo damagee dmg

    doDamageTo :: Ent -> Int -> GameSystem ()
    doDamageTo ent dmg = forEnt ent $ do
      hp <- get health
      guard (dmg > 0)
      pure $ defEntity' { health = Set $ hp - dmg }


killDeadEntities :: GameSystem ()
killDeadEntities = void $ emap $ do
  hp <- get health
  guard (hp <= 0)
  pure delEntity
