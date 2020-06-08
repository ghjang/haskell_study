module Utility
( getMaybe
, rotateLeft
) where

import Data.Monoid
  
getMaybe :: Maybe a -> a
getMaybe (Just x) = x

rotateLeft :: Int -> [a] -> [a]
rotateLeft = drop <> take
