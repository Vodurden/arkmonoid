module Arkmonoid.Power.Types where

data Power = Widen | Shorten

newtype PowerApplier = PowerApplier Power
newtype PowerSpawner = PowerSpawner Power
