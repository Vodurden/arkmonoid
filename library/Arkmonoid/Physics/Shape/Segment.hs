module Arkmonoid.Physics.Shape.Segment where

import Control.Monad
import Control.Lens
import Linear.V2
import Linear.Metric
import Linear.Epsilon

import           Arkmonoid.Physics.Shape.Types
import qualified Arkmonoid.Physics.Shape.Line as Line

normalVector :: Segment -> V2 Float
normalVector = Line.normalVector . Line.fromSegment

length :: Segment -> Float
length (Segment start end) = distance start end

center :: Segment -> Point
center (Segment start end) = (start + end) / 2.0

toLine :: Segment -> Line
toLine (Segment start end) = Line start end

-- | Returns true if the point is on the segment
containsPoint :: Segment -> Point -> Bool
containsPoint (Segment start end) point =
  nearZero $ distance start point + distance point end - distance start end

-- | Finds the intersection of two segments if it exists
intersection :: Segment -> Segment -> Maybe Point
intersection seg1 seg2 = do
  lineIntersectionPoint <- Line.intersection (Line.fromSegment seg1) (Line.fromSegment seg2)
  guard (containsPoint seg1 lineIntersectionPoint)
  guard (containsPoint seg2 lineIntersectionPoint)
  pure lineIntersectionPoint

-- | Calculates the fraction of the segment from start to the given point.
-- | If the point is on the line this will return between 0.0 - 1.0. Otherwise
-- | It will return Nothing
intersectionFraction :: Segment -> Point -> Maybe Float
intersectionFraction seg point
  | containsPoint seg point =
      let intersectionSegment = Segment (seg^.startPoint) point
          intersectionLength = Arkmonoid.Physics.Shape.Segment.length intersectionSegment
          segmentLength = Arkmonoid.Physics.Shape.Segment.length seg
      in Just $ intersectionLength / segmentLength
  | otherwise = Nothing

-- | Finds the intersection point of a segment and line if it exists
lineIntersection :: Segment -> Line -> Maybe Point
lineIntersection segment line = do
  -- First we find if there's an intersection between the two infinite lines
  -- if there is we just need to check if it's within our segment
  lineSegIntersectionPoint <- Line.intersection (Line.fromSegment segment) line
  guard (containsPoint segment lineSegIntersectionPoint)
  pure lineSegIntersectionPoint
