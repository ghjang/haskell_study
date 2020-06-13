{-# LANGUAGE FlexibleContexts #-}

module Math.PrimeNumber
( primes
) where
  
import Control.Monad.State

primes :: [Int]
primes = fst $ runState (sequence $ repeat nextPrime) $ [2..]
  where
    nextPrime = state $ \(x:xs) -> (x, filter (\y -> y `mod` x /= 0) xs)
