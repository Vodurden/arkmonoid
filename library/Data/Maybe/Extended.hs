module Data.Maybe.Extended
  ( module Data.Maybe
  , applyOrOther
  ) where

import Data.Maybe

applyOrOther :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
applyOrOther f (Just x) (Just y) = Just $ f x y
applyOrOther _ (Just x) Nothing = Just x
applyOrOther _ Nothing (Just y) = Just y
applyOrOther _ Nothing Nothing = Nothing
