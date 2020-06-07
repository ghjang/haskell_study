module Utility
( getMaybe
) where
  
getMaybe :: Maybe a -> a
getMaybe (Just x) = x
