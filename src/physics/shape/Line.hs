module Physics.Shape.Line where

import Physics.Shape.Types

import Linear.V2
import Linear.V3
import Linear.Matrix
import Linear.Epsilon

fromSegment :: Segment -> Line
fromSegment (Segment start end) = Line start end

-- | the `a` portion of the linear equation of this line: ax + by + c = 0
a :: Line -> Float
a (Line (V2 _ y1) (V2 _ y2)) = y1 - y2

-- | the `b` portion of the linear equation of this line: ax + by + c = 0
b :: Line -> Float
b (Line (V2 x1 _) (V2 x2 _)) = x2 - x1

-- | c portion of the linear equation of this line: ax + by + c = 0
c :: Line -> Float
c (Line (V2 x1 y1) (V2 x2 y2)) = (x1 * y2) - (x2 * y1)

dx :: Line -> Float
dx = b

dy :: Line -> Float
dy = a

normalVector :: Line -> V2 Float
normalVector line = V2 (-dy line) (dx line)

determinant :: Line -> Line -> Float
determinant line1 line2 = (a line1) * (b line2) - (a line2) * (b line1)

-- | This is not philosophically correct but we take the center of an infinite
-- | line as the midpoint between the two definition points.
-- |
-- | In reality the midpoint of an infinite line is 0 on at least one axis. But we're going to ignore that :)
center :: Line -> Point
center (Line start end) = (start + end) / 2.0

-- | Finds the intersection point of two lines if it exists
intersection :: Line -> Line -> Maybe Point
intersection line1 line2
    | nearZero det = Nothing
    | otherwise =
        let intersectionX = (b line2 * (negate $ c line1) - b line1 * (negate $ c line2)) / det
            intersectionY = (a line1 * (negate $ c line2) - a line2 * (negate $ c line1)) / det
        in Just $ V2 intersectionX intersectionY
  where
    det = determinant line1 line2

-- | Returns the area of the triangle formed by the start of the line, end of the line
-- | and the given point
triangularArea :: Line -> Point -> Float
triangularArea (Line (V2 startX startY) (V2 endX endY)) (V2 pointX pointY) =
    -- The determinant is the area of the triangle formed by these three points.
    det33 matrix
  where
    matrix = V3
      (V3 startX startY 1.0)
      (V3 endX   endY   1.0)
      (V3 pointX pointY 1.0)

containsPoint :: Line -> Point -> Bool
containsPoint line point =
    -- If the area of the triangle formed by these three points is near zero then we know
    -- the points are co-linear: the line contains the point
    nearZero (triangularArea line point)
