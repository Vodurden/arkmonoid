{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Arkmonoid.Types where

import           Control.Monad.Random
import           Data.Ecstasy
import qualified Graphics.Gloss.Data.Color as G

import Arkmonoid.Physics.Types
import Arkmonoid.Power.Types
import Arkmonoid.Mortality.Types

data FollowMouse = FollowMouse Bool Bool -- Follow mouse on the X-axis/Y-axis/both

data Entity' f = Entity
  { physicalObject  :: Component f 'Field PhysicalObject

  , mortality       :: Component f 'Field Mortality
  , damage          :: Component f 'Field Damage

  , powerApplier    :: Component f 'Field PowerApplier
  , powerSpawner    :: Component f 'Field PowerSpawner
  , powerReceiver   :: Component f 'Field ()

  , color           :: Component f 'Field G.Color
  , followMouse     :: Component f 'Field FollowMouse
  , debug           :: Component f 'Field ()
  }
  deriving (Generic)

data GameState = GameState
  { _gameState :: GameSystemState Underlying
  , _random :: StdGen
  }

type GameSystemState m = SystemState Entity' m

type GameSystemT m a = SystemT Entity' m a
type GameQueryT m a = QueryT Entity' m a

type Underlying = (RandT StdGen IO)
newtype Arkmonad a = Arkmonad { runArkmonad :: (GameSystemT Underlying a) }
  deriving (Functor, Applicative, Monad, MonadIO)

-- Todo find a better way to expose these globals
screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480
