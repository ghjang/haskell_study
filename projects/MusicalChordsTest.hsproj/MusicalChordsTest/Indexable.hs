{-# LANGUAGE ScopedTypeVariables #-}

module Indexable
( Indexable (..)
) where

import Data.List

class (Eq a, Bounded a, Enum a) => Indexable a where
  indexOf :: a -> Int
  indexOf e = fst . head $ dropWhile (\(x, y) -> e /= y)
                         $ zip [0..] [(minBound :: a) .. (maxBound :: a)]
                         
  fromIndex :: Int -> Maybe a
  fromIndex n = foldl f Nothing $ zip [0..] [(minBound :: a) .. (maxBound :: a)]
    where
      f (Just e) _ = Just e
      f Nothing (x, y) = if n == x then Just y else Nothing
