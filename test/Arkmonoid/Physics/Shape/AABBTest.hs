module Arkmonoid.Physics.Shape.AABBTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Linear.V2

import Arkmonoid.Physics.Shape.Types
import Arkmonoid.Physics.Shape.AABB

unit_scaleWidth_works_on_growing_scalars :: IO ()
unit_scaleWidth_works_on_growing_scalars =
  let shape = AABB { _minPoint = V2 0 0, _maxPoint = V2 100 100 }
      result = scaleWidth 1.1 shape
      expected = AABB
        { _minPoint = V2 (-5) 0, _maxPoint = V2 105 100 }
  in assertEqual "for (scaleWidth 1.1 shape)" expected result


unit_scaleWidth_works_on_shrinking_scalars :: IO ()
unit_scaleWidth_works_on_shrinking_scalars =
  let shape = AABB { _minPoint = V2 0 0, _maxPoint = V2 100 100 }
      result = scaleWidth (0.9) shape
      expected = AABB
        { _minPoint = V2 5 0, _maxPoint = V2 95 100 }
  in assertEqual "for (scaleWidth (-1.1) shape)" expected result
