module Arkmonoid.Mortality.Mortality where

import Arkmonoid.Mortality.Types

damage :: Damage -> Mortality -> Mortality
damage Harmless m = m
damage (DamageOnCollision dmg) m = damageBy dmg m

damageBy :: Health -> Mortality -> Mortality
damageBy _ Immortal = Immortal
damageBy _ Dead = Dead
damageBy dmg (Mortal health)
  | health - dmg <= 0 = Dead
  | otherwise = Mortal (health - dmg)
