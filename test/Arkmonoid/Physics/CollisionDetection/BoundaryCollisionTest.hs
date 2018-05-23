module Arkmonoid.Physics.CollisionDetection.BoundaryCollisionTest where

import           Test.Tasty.HUnit

import Linear.V2.Extended

import Arkmonoid.Physics.CollisionDetection.BoundaryCollision
import Arkmonoid.Physics.Types
import Arkmonoid.Physics.Shape.Types

unit_pointCollision_detects_points_beyond_boundary :: IO ()
unit_pointCollision_detects_points_beyond_boundary =
    assertEqual "for pointCollision (RightBoundary ...) (Point 10 0)" expected result
  where
    expected = Just (V2 (-10) 0)
    result = pointCollision boundary (V2 10 0)

    boundary = RightBoundary (Line startP endP) Solid
    startP = V2 0 0
    endP = V2 0 1
