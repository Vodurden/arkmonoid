{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Ecstasy
import Linear.V2
import qualified Graphics.Gloss.Data.Color as G

data Geometry = Box Float Float   -- A rectangle with a width and height

data FollowMouse = FollowMouse Bool Bool -- Follow mouse on the X-axis/Y-axis/both

data Entity' f = Entity
  { position        :: Component f 'Field (V2 Float)
  , velocity        :: Component f 'Field (V2 Float)
  , impulse         :: Component f 'Field (V2 Float)
  , speed           :: Component f 'Field (V2 Float) -- ^ derived from velocity and impulse
  , frozen          :: Component f 'Field ()
  , geometry        :: Component f 'Field Geometry
  , health          :: Component f 'Field Int -- ^ amount of damage this entity can sustain
  , damage          :: Component f 'Field Int -- ^ amount of damage this entity does on collision
  , color           :: Component f 'Field G.Color
  , bouncy          :: Component f 'Field ()
  , followMouse     :: Component f 'Field FollowMouse
  , debug           :: Component f 'Field ()
  }
  deriving (Generic)

type World = (Int, Entity' 'WorldOf)
type GameSystem a = System Entity' a

type GameQueryT m a = QueryT Entity' m a

-- Todo find a better way to expose these globals
screenWidth = 640
screenHeight = 480
