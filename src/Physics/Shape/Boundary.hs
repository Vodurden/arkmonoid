module Physics.Shape.Boundary where

import Physics.Shape.Types
import qualified Physics.Shape.AABB as AABB
import qualified Physics.Shape.Line as Line
import qualified Physics.Shape.CollisionModel as CollisionModel

import Extra.Ord
import Extra.Maybe

import Linear.Metric
import Control.Applicative

collision :: Boundary -> CollisionModel -> Maybe CollisionType
collision boundary model = staticCollision boundary model <|> dynamicCollision boundary model

-- | Assumes that the collision model is _already_ past the boundary
staticCollision :: Boundary -> CollisionModel -> Maybe CollisionType
staticCollision boundary model = do
  let box = CollisionModel.box model
  let minCollision = staticPointCollision boundary (AABB.minPoint box)
  let maxCollision = staticPointCollision boundary (AABB.maxPoint box)
  let largestPenetration = applyOrOther (maxBy quadrance) minCollision maxCollision
  fmap PenetrationCollision largestPenetration

staticPointCollision :: Boundary -> Point -> Maybe PenetrationVector
staticPointCollision boundary @ (Boundary line _) position | staticPointColliding boundary position = do
    let movementLine = Line position (position + Line.normalVector line)
    intersection <- Line.intersection line movementLine
    let penetrationVector = intersection - position
    pure penetrationVector
staticPointCollision _ _ = Nothing

staticPointColliding :: Boundary -> Point -> Bool
staticPointColliding (Boundary line BoundLeft) point  = Line.pointDeterminant line point > 0
staticPointColliding (Boundary line BoundRight) point = Line.pointDeterminant line point < 0

-- TODO: Implement
dynamicCollision :: Boundary -> CollisionModel -> Maybe CollisionType
dynamicCollision _ _ = Nothing
