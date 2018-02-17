{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Ecstasy
import Linear.V2
import qualified Graphics.Gloss.Data.Color as G

data Geometry = Ball Float              -- A ball with a radius
              | Rectangle Float Float   -- A rectangle with a width and height

data Entity' f = Entity
  { position :: Component f 'Field (V2 Float)
  , velocity :: Component f 'Field (V2 Float)
  , geometry :: Component f 'Field Geometry
  , color    :: Component f 'Field G.Color
  }
  deriving (Generic)

type World = (Int, Entity' 'WorldOf)
type GameSystem a = System Entity' a
