module Physics.CollisionDetection.BoundaryCollision where

import Extra.Ord
import Extra.Maybe
import Physics.Types
import Physics.CollisionDetection.Types
import Physics.Shape.Types
import qualified Physics.Shape.Line as Line

import Control.Lens
import Data.Maybe (catMaybes)
import Linear.Metric

gameCollisions :: GameObject id -> [Boundary] -> [GameBoundaryCollision id]
gameCollisions obj boundaries =
  catMaybes $ flip fmap boundaries $ \boundary -> gameCollision boundary obj

-- | Finds a collision between a game object and a boundary if it exists
gameCollision :: Boundary -> GameObject id -> Maybe (GameBoundaryCollision id)
gameCollision boundary obj = do
  boundsCollision <- collision boundary (obj^.physical)
  pure $ GameBoundaryCollision (obj^.identifier) boundsCollision

-- | Detects a collision between the boundary and a physics object.
collision :: Boundary -> PhysicalObject -> Maybe BoundaryCollision
collision boundary obj = do
  let box = obj^.shape
  let minCollision = pointCollision boundary (box^.minPoint)
  let maxCollision = pointCollision boundary (box^.maxPoint)
  let largestPenetration = applyOrOther (maxBy quadrance) minCollision maxCollision
  BoundaryCollision obj <$> largestPenetration

-- | Detects if a point is beyond the boundary and returns the resulting collision
pointCollision :: Boundary -> Point -> Maybe PenetrationVector
pointCollision boundary pos | pointBeyondBoundary boundary pos = do
  let line = boundaryLine boundary
  let movementLine = Line pos (pos + Line.normalVector line)
  intersection <- Line.intersection line movementLine
  let penetrationV = intersection - pos
  pure penetrationV
pointCollision _ _ = Nothing

-- | Returns true if the point is beyond the boundary
pointBeyondBoundary :: Boundary -> Point -> Bool
pointBeyondBoundary (LeftBoundary  line _) p = Line.pointDeterminant line p > 0
pointBeyondBoundary (RightBoundary line _) p = Line.pointDeterminant line p < 0

boundaryLine :: Boundary -> Line
boundaryLine (LeftBoundary line _) = line
boundaryLine (RightBoundary line _) = line
