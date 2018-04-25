{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Arkmonoid.Physics.CollisionDetection.Detection where

import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Control.Lens

import           Arkmonoid.Extra.List (pairs)
import           Arkmonoid.Extra.Ord (minBy)
import           Arkmonoid.Physics.Types
import           Arkmonoid.Physics.CollisionDetection.Types
import qualified Arkmonoid.Physics.CollisionDetection.BoundaryCollision as BoundaryCollision
import qualified Arkmonoid.Physics.CollisionDetection.ExistingCollision as ExistingCollision
import qualified Arkmonoid.Physics.CollisionDetection.ImpendingCollision as ImpendingCollision


-- | Detects collisions between a world of objects surrounded by the given boundaries.
-- |
-- | Object collisions are prioritised by the following rules:
-- |
-- | For each object:
-- |
-- |     1. If we are colliding with one or more boundaries then that takes priority
-- |     2. Otherwise, if we have any existing collisions those take priority
-- |     3. Otherwise, the _shortest_ impending collision takes priority
-- |
-- | This assumes that you want to resolve existing collisions before looking for any
-- | impending collisions
collisions :: forall id. (Ord id) => Float -> [Boundary] -> [GameObject id] -> GameCollisions id
collisions delta boundaries objects =
    Map.fromListWith mergeGameCollision $ concat
      [ fmap withKey $ boundaryCollisions boundaries objects
      , fmap withKey $ existingCollisions objects
      , fmap withKey $ impendingCollisions delta objects
      ]
  where
    withKey :: GameCollision id -> (id, GameCollision id)
    withKey c @ (GBoundaryCollision id _)  = (id, c)
    withKey c @ (GExistingCollision id _)  = (id, c)
    withKey c @ (GImpendingCollision id _) = (id, c)

    -- | This function asssumes that both game collisions are for the same object
    mergeGameCollision :: GameCollision id -> GameCollision id -> GameCollision id
    mergeGameCollision (GBoundaryCollision id cs1) (GBoundaryCollision _ cs2)        = GBoundaryCollision id (cs1 ++ cs2)
    mergeGameCollision (GBoundaryCollision id cs) _                                  = GBoundaryCollision id cs
    mergeGameCollision _ (GBoundaryCollision id cs)                                  = GBoundaryCollision id cs
    mergeGameCollision (GExistingCollision id cs1) (GExistingCollision _ cs2)        = GExistingCollision id (cs1 ++ cs2)
    mergeGameCollision (GExistingCollision id cs) _                                  = GExistingCollision id cs
    mergeGameCollision _ (GExistingCollision id cs)                                  = GExistingCollision id cs
    mergeGameCollision (GImpendingCollision id current) (GImpendingCollision _ new)  = GImpendingCollision id (minBy (^.impendingCollision.objectDistance) current new)

boundaryCollisions :: [Boundary] -> [GameObject id] -> [GameCollision id]
boundaryCollisions boundaries objects = do
  obj <- objects
  boundary <- boundaries
  maybeToList $ fmap (\b -> GBoundaryCollision (obj^.identifier) [b]) (BoundaryCollision.gameCollision boundary obj)

existingCollisions :: forall id. [GameObject id] -> [GameCollision id]
existingCollisions objects = concatMap (uncurry existingCollision) (pairs objects)
  where existingCollision :: GameObject id -> GameObject id -> [GameCollision id]
        existingCollision obj1 obj2 = do
          collision <- maybeToList $ ExistingCollision.gameCollision obj1 obj2
          let flipped = ExistingCollision.gameFlipExisting collision
          [GExistingCollision (collision^.objectId) [collision], GExistingCollision (flipped^.objectId) [flipped]]

impendingCollisions :: forall id. Float -> [GameObject id] -> [GameCollision id]
impendingCollisions delta objects = concatMap (uncurry impendingCollision) (pairs objects)
  where impendingCollision :: GameObject id -> GameObject id -> [GameCollision id]
        impendingCollision obj1 obj2 = do
          collision <- maybeToList $ ImpendingCollision.gameCollision delta obj1 obj2
          let flipped = ImpendingCollision.gameFlipImpending collision
          [GImpendingCollision (collision^.objectId) collision, GImpendingCollision (flipped^.objectId) flipped]
