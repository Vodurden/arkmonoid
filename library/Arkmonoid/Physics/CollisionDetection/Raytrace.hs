module Arkmonoid.Physics.CollisionDetection.Raytrace where

import Data.Maybe (catMaybes, listToMaybe)
import Data.List (sortOn)
import Control.Lens
import qualified Linear.Metric as Metric

import           Arkmonoid.Physics.CollisionDetection.Types
import           Arkmonoid.Physics.Shape.Types
import qualified Arkmonoid.Physics.Shape.AABB as AABB
import qualified Arkmonoid.Physics.Shape.Segment as Segment

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
