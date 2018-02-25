module Simulate.Collision where

import Data.Maybe
import Data.Ecstasy
import Linear.V2

import Types
import qualified Simulate.Shape as S

-- | Describes a collision between two entities, or between a single entity and the boundary
-- | of the world
data Collision = EntityCollision Ent Impact Ent Impact
               | BoundaryCollision Ent Impact
               deriving (Show, Eq)

-- | Describes a collision from the perspective of a single entity
-- |
-- | Has the side the collision occurs and the location of the collision
data Impact = Impact Side (V2 Float)
  deriving (Show, Eq)

-- | The side of a rectangle
data Side = TopSide | BottomSide | LeftSide | RightSide
  deriving (Show, Eq)

-- | Finds boundary collisions given the width/height of the world.
-- |
-- | We assume that (0, 0) is the center of the world
boundaryCollisions :: Float -> Float -> GameSystem [Collision]
boundaryCollisions width height =
  fmap concat $ efor $ \ent -> do
    shape <- S.fromEntity
    let impacts = boundaryImpacts width height shape
    let collisions = fmap (BoundaryCollision ent) impacts
    pure $ collisions

boundaryImpacts :: Float -> Float -> S.Shape -> [Impact]
boundaryImpacts width height shape = catMaybes
  [ leftBoundaryCollision   (negate (width / 2))  shape
  , rightBoundaryCollision  (width / 2)           shape
  , topBoundaryCollision    (height / 2)          shape
  , bottomBoundaryCollision (negate (height / 2)) shape
  ]

leftBoundaryCollision :: Float -> S.Shape -> Maybe Impact
leftBoundaryCollision minX shape =
  if (S.left shape) < minX
  then Just $ Impact LeftSide (V2 minX (S.y shape))
  else Nothing

rightBoundaryCollision :: Float -> S.Shape -> Maybe Impact
rightBoundaryCollision maxX shape =
  if (S.right shape) > maxX
  then Just $ Impact RightSide (V2 maxX (S.y shape))
  else Nothing

topBoundaryCollision :: Float -> S.Shape -> Maybe Impact
topBoundaryCollision maxY shape =
  if (S.top shape) > maxY
  then Just $ Impact TopSide (V2 (S.x shape) maxY)
  else Nothing

bottomBoundaryCollision :: Float -> S.Shape -> Maybe Impact
bottomBoundaryCollision minY shape =
  if (S.bottom shape) < minY
  then Just $ Impact BottomSide (V2 (S.x shape) minY)
  else Nothing
