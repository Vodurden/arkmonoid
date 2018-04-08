module Physics.CollisionDetection.Raytrace where

import           Physics.CollisionDetection.Types
import           Physics.Shape.Types
import qualified Physics.Shape.AABB as AABB
import qualified Physics.Shape.Segment as Segment

import Data.Maybe (catMaybes, listToMaybe)
import Data.List (sortOn)
import Control.Lens
import qualified Linear.Metric as Metric

-- | Raytrace against an AABB.
traceAABB :: Ray -> AABB -> Maybe RaytraceCollision
traceAABB ray aabb =
  let intersections = catMaybes $ fmap (traceSegment ray) (AABB.segments aabb)
      orderedIntersections = sortOn (^.distance) intersections
      smallestIntersection = listToMaybe orderedIntersections
  in smallestIntersection

-- | Raytrace against a segment.
traceSegment :: Ray -> Segment -> Maybe RaytraceCollision
traceSegment ray seg = do
  intersectionPoint <- Segment.intersection ray seg
  let traceDistance = Metric.distance (ray^.startPoint) intersectionPoint
  let remaining = Metric.distance intersectionPoint (ray^.endPoint)
  pure $ RaytraceCollision intersectionPoint seg traceDistance remaining
