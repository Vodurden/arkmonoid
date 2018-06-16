{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

module Arkmonoid.Types where

import           Data.Ecstasy
import qualified Graphics.Gloss.Data.Color as G

import Arkmonoid.Physics.Types
import Arkmonoid.Mortality.Types

data FollowMouse = FollowMouse Bool Bool -- Follow mouse on the X-axis/Y-axis/both

data Entity' f = Entity
  { entId           :: Component f 'Field Ent
  , physicalObject  :: Component f 'Field PhysicalObject
  , mortality       :: Component f 'Field Mortality
  , damage          :: Component f 'Field Damage
  , color           :: Component f 'Field G.Color
  , followMouse     :: Component f 'Field FollowMouse
  , debug           :: Component f 'Field ()
  }
  deriving (Generic)

type World = (Int, Entity' 'WorldOf)
type GameSystem a = System Entity' a

type GameQueryT m a = QueryT Entity' m a

-- Todo find a better way to expose these globals
screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480
