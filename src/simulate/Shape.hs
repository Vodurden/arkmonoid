module Simulate.Shape where

import Data.Ecstasy
import Linear.V2

import Types

type Position = V2 Float
type Size = V2 Float
data Shape = Rectangle Position Size

fromEntity :: (Monad world) => GameQueryT world Shape
fromEntity = do
    pos <- get position
    geo <- get geometry
    pure $ fromEntity' pos geo
  where
    fromEntity' :: (V2 Float) -> Geometry -> Shape
    fromEntity' (V2 x y) (Box w h) = Rectangle (V2 x y) (V2 w h)

x :: Shape -> Float
x (Rectangle (V2 x _) _) = x

y :: Shape -> Float
y (Rectangle (V2 _ y) _) = y

w :: Shape -> Float
w (Rectangle _ (V2 w _)) = w

h :: Shape -> Float
h (Rectangle _ (V2 _ h)) = h

left :: Shape -> Float
left (Rectangle (V2 x _) (V2 w _)) = x - (w / 2)

right :: Shape -> Float
right (Rectangle (V2 x _) (V2 w _)) = x + (w / 2)

top :: Shape -> Float
top (Rectangle (V2 _ y) (V2 _ h)) = y + (h / 2)

bottom :: Shape -> Float
bottom (Rectangle (V2 _ y) (V2 _ h)) = y - (h / 2)
