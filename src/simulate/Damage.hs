module Simulate.Damage where

import Control.Monad
import Data.Maybe

import Data.Ecstasy
import Extra.Ecstasy

import Types

onEntityCollision :: Ent -> Ent -> GameSystem ()
onEntityCollision ent1 ent2 = do
    maybeDamage1 <- eget ent1 (get damage)
    maybeDamage2 <- eget ent2 (get damage)

    doDamage ent1 maybeDamage2
    doDamage ent2 maybeDamage1
  where
    doDamage ent maybeDamage = forEnt ent $ do
      hp <- get health
      let dmg = fromMaybe 0 maybeDamage
      pure $ defEntity' { health = Set $ hp - dmg }

step :: GameSystem ()
step = killDeadEntities

killDeadEntities :: GameSystem ()
killDeadEntities = void $ emap $ do
  hp <- get health
  if hp <= 0
  then pure delEntity
  else pure defEntity'
