{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds     #-}

module Extra.Ecstasy where

import Data.Foldable (for_)
import Data.Ecstasy
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S

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

emapIndexed :: (HasWorld world, Monad m)
            => (Ent -> QueryT world m (world 'SetterOf))
            -> SystemT world m ()
emapIndexed f = do
  (es, _) <- S.get
  for_ [0 .. es - 1] $ \(Ent -> e) -> do
    cs <- getEntity e
    sets <- lift $ unQueryT (f e) cs
    for_ sets $ setEntity e
