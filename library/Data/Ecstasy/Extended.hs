{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE UndecidableInstances #-}

-- GHC seems to want us to use the expanded generics from
-- HasWorld. They're huge so we're not going to do that
-- here.
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Data.Ecstasy.Extended
  ( module Data.Ecstasy
  , forEnt
  , eget
  ) where

import           Data.Foldable (for_)
import           Data.Ecstasy
import           Data.Ecstasy.Internal (HasWorld, unQueryT)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S

forEnt :: (HasWorld world m, Monad m)
       => Ent
       -> QueryT world m (world 'SetterOf)
       -> SystemT world m ()
forEnt ent q = do
  sets <- runQueryT ent q
  for_ sets $ setEntity ent

eget :: (HasWorld world m, Monad m)
     => Ent
     -> QueryT world m a
     -> SystemT world m (Maybe a)
eget ent q = runQueryT ent q
