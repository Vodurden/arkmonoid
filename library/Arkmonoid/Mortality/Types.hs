module Arkmonoid.Mortality.Types where

-- | amount of damage an entity can sustain
type Health = Int

data Mortality = Immortal
               | Mortal Health
               deriving Eq

data Damage = DamageOnCollision Health
            | Harmless
            deriving Eq
