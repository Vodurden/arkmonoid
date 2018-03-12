module Simulate.Line where

-- | Represents an infinite line passing through both points
data Line = Line Point Point

-- | Represents a finite line between both points
data Segment = Segment Point Point
