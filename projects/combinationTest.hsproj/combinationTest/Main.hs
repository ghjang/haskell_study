{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Writer

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f = filterM g
  where
    g x = if f x then writer $ (False, [x])
                 else writer $ (True, [])

powerset :: [a] -> [[a]]
powerset = filterM (\x -> [True, False])

combination :: [a] -> Int -> [[a]]
combination xs r = filter (\ys -> length ys == r) $ powerset xs
