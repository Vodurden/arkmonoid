{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Ecstasy
import Linear.V2
import qualified Graphics.Gloss.Data.Color as G

data Geometry = Box Float Float   -- A rectangle with a width and height

data Entity' f = Entity
  { position :: Component f 'Field (V2 Float)
  , velocity :: Component f 'Field (V2 Float)
  , geometry :: Component f 'Field Geometry
  , color    :: Component f 'Field G.Color
  , bouncy   :: Component f 'Field ()
  }
  deriving (Generic)

type World = (Int, Entity' 'WorldOf)
type GameSystem a = System Entity' a

type GameQueryT m a = QueryT Entity' m a

-- Todo find a better way to expose these globals
screenWidth = 640
screenHeight = 480

-- runQuery :: Ent -> Query world a -> System world (Maybe a)
-- runQuery ent = runQueryT
-- runQueryT
--     :: ( HasWorld world
--        , Monad m
--        )
--     => Ent
--     -> QueryT world m a
--     -> SystemT world m (Maybe a)
-- runQueryT e qt = do
--   cs <- getEntity e
-- lift $ unQueryT qt cs

-- runQuery
--     :: ( HasWorld world)
--     => Ent
--     -> Query world a
--     -> System world (Maybe a)
-- runQuery e qt = do
--   cs <- getEntity e
--   lift $ unQuery qt cs
