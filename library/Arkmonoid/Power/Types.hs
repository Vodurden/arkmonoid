module Arkmonoid.Power.Types where

data Power = Widen | Shorten | MultiBall

newtype PowerApplier = PowerApplier Power
newtype PowerSpawner = PowerSpawner Power
