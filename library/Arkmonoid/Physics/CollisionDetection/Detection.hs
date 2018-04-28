{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Arkmonoid.Physics.CollisionDetection.Detection where

import           Control.Lens
import           Data.List.Extended
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
import           Data.Ord.Extended (minBy)

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
      [ withKey <$> boundaryCollisions boundaries objects
      , withKey <$> existingCollisions objects
      , withKey <$> impendingCollisions delta objects
      ]
  where
    withKey :: GameCollision id -> (id, GameCollision id)
    withKey c @ (GBoundaryCollision objId _)  = (objId, c)
    withKey c @ (GExistingCollision objId _)  = (objId, c)
    withKey c @ (GImpendingCollision objId _) = (objId, c)

    -- | This function asssumes that both game collisions are for the same object
    mergeGameCollision :: GameCollision id -> GameCollision id -> GameCollision id
    mergeGameCollision (GBoundaryCollision objId cs1) (GBoundaryCollision _ cs2)        = GBoundaryCollision objId (cs1 ++ cs2)
    mergeGameCollision (GBoundaryCollision objId cs) _                                  = GBoundaryCollision objId cs
    mergeGameCollision _ (GBoundaryCollision objId cs)                                  = GBoundaryCollision objId cs
    mergeGameCollision (GExistingCollision objId cs1) (GExistingCollision _ cs2)        = GExistingCollision objId (cs1 ++ cs2)
    mergeGameCollision (GExistingCollision objId cs) _                                  = GExistingCollision objId cs
    mergeGameCollision _ (GExistingCollision objId cs)                                  = GExistingCollision objId cs
    mergeGameCollision (GImpendingCollision objId current) (GImpendingCollision _ new)  = GImpendingCollision objId (minBy (^.impendingCollision.objectDistance) current new)

boundaryCollisions :: [Boundary] -> [GameObject id] -> [GameCollision id]
boundaryCollisions boundaries objects = do
  obj <- objects
  boundary <- boundaries
  maybeToList $ fmap (\b -> GBoundaryCollision (obj^.identifier) [b]) (BoundaryCollision.gameCollision boundary obj)

existingCollisions :: forall id. [GameObject id] -> [GameCollision id]
existingCollisions objects = concatMap (uncurry existingGameCollision) (pairs objects)
  where existingGameCollision :: GameObject id -> GameObject id -> [GameCollision id]
        existingGameCollision obj1 obj2 = do
          collision <- maybeToList $ ExistingCollision.gameCollision obj1 obj2
          let flippedCollision = ExistingCollision.gameFlipExisting collision
          let gameCollision = GExistingCollision (collision^.objectId) [collision]
          let flippedGameCollision = GExistingCollision (flippedCollision^.objectId) [flippedCollision]
          [gameCollision, flippedGameCollision]

impendingCollisions :: forall id. Float -> [GameObject id] -> [GameCollision id]
impendingCollisions delta objects = concatMap (uncurry impendingGameCollision) (pairs objects)
  where impendingGameCollision :: GameObject id -> GameObject id -> [GameCollision id]
        impendingGameCollision obj1 obj2 = do
          collision <- maybeToList $ ImpendingCollision.gameCollision delta obj1 obj2
          let flippedCollision = ImpendingCollision.gameFlipImpending collision
          let gameCollision = GImpendingCollision (collision^.objectId) collision
          let flippedGameCollision = GImpendingCollision (flippedCollision^.objectId) flippedCollision
          [gameCollision, flippedGameCollision]
