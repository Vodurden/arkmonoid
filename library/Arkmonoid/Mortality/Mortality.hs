module Arkmonoid.Mortality.Mortality where

import Arkmonoid.Mortality.Types

damage :: Damage -> Mortality -> Mortality
damage _ Immortal = Immortal
damage Harmless m = m
damage (DamageOnCollision dmg) (Mortal health) = Mortal (health - dmg)
