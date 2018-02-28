module Simulate.Collision where

import Data.Maybe
import Data.List
import Data.Ecstasy
import Linear.V2

import Types
import Extra.List
import qualified Simulate.Shape as S

-- | Describes a collision between two entities, or between a single entity and the boundary
-- | of the world
data Collision = EntityCollision Ent Ent Impact Impact
               | BoundaryCollision Ent Impact
               deriving (Show, Eq)

-- | Describes a collision from the perspective of a single entity
-- |
-- | Contints a vector representing the depth of penetration
data Impact = Impact (V2 Float)
  deriving (Show, Eq)

-- | The side of a rectangle
data Side = TopSide | BottomSide | LeftSide | RightSide
  deriving (Show, Eq)

-- | Finds all collisions given the width/height of the world.
-- |
-- | We assume that (0, 0) is the center of the world
collisions :: Float -> Float -> GameSystem [Collision]
collisions width height =
  (++) <$> boundaryCollisions width height
       <*> entityCollisions

entityCollisions :: GameSystem [Collision]
entityCollisions = do
  candidates <- efor $ \ent -> do
    shape <- S.fromEntity
    pure (ent, shape)
  let allCollisions = pairs candidates
  pure $ catMaybes $ fmap (uncurry shapeCollision) allCollisions

shapeCollision :: (Ent, S.Shape) -> (Ent, S.Shape) -> Maybe Collision
shapeCollision (ent1, shape1) (ent2, shape2) =
  fmap (uncurry (EntityCollision ent1 ent2)) (shapeImpact shape1 shape2)

shapeImpact :: S.Shape -> S.Shape -> Maybe (Impact, Impact)
shapeImpact s1 s2 = fmap (\p -> (Impact (p / 2), Impact $ negate (p / 2))) (S.penetration s1 s2)


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
  then Just $ Impact (V2 (minX - S.left shape) 0)
  else Nothing

rightBoundaryCollision :: Float -> S.Shape -> Maybe Impact
rightBoundaryCollision maxX shape =
  if (S.right shape) > maxX
  then Just $ Impact (V2 (maxX - S.right shape) 0)
  else Nothing

topBoundaryCollision :: Float -> S.Shape -> Maybe Impact
topBoundaryCollision maxY shape =
  if (S.top shape) > maxY
  then Just $ Impact (V2 0 (maxY - S.top shape))
  else Nothing

bottomBoundaryCollision :: Float -> S.Shape -> Maybe Impact
bottomBoundaryCollision minY shape =
  if (S.bottom shape) < minY
  then Just $ Impact (V2 0 (minY - S.bottom shape))
  else Nothing
