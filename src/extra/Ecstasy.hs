{-# LANGUAGE DataKinds     #-}

module Extra.Ecstasy where

import Data.Foldable (for_)
import Data.Ecstasy
import Control.Monad.Trans.Class (lift)

forEnt :: (HasWorld world, Monad m)
       => Ent
       -> QueryT world m (world 'SetterOf)
       -> SystemT world m ()
forEnt ent query = do
  cs <- getEntity ent
  sets <- lift $ unQueryT query cs
  for_ sets $ setEntity ent

eget :: (HasWorld world, Monad m)
     => Ent
     -> QueryT world m a
     -> SystemT world m (Maybe a)
eget ent query = do
  cs <- getEntity ent
  lift $ unQueryT query cs
