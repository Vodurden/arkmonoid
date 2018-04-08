module Damage.DamageSystem where

import Control.Monad
import Data.Ecstasy
import Data.Foldable
import Data.Maybe
import Extra.Ecstasy

import           Types
import           Physics.CollisionDetection.Types
import qualified Physics.CollisionDetection.GameCollisions as GameCollisions

step :: GameCollisions Ent -> GameSystem ()
step collisions = do
  damageCollidingEntities collisions
  killDeadEntities

damageCollidingEntities :: GameCollisions Ent -> GameSystem ()
damageCollidingEntities collisions =
    let collidingEnts = GameCollisions.collidingIds collisions
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
