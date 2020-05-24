module Code_2_4 () where

import Control.Monad.State

-- by using top-down recursion
fibo :: Int -> Int
fibo 1 = 1
fibo 2 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

-- by using a bottom-up approach with the 'foldl' function
fibo' :: Int -> Int
fibo' 1 = 1
fibo' 2 = 1
fibo' n = head $ foldl f [1,1] [1..(n - 2)]
  where
    f (a:b:fs) _ = (a + b):(a:b:[])

-- by using corecursion
fibo'' :: Int -> Int
fibo'' n = head . drop (n - 1) $ fibs
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- by using State Monad
type LastTwo = (Int, Int)

nextFibo :: State LastTwo Int
nextFibo = state $ \(a, b) -> (a + b, (b, a + b))

buildFiboState :: Int -> State LastTwo [Int]
buildFiboState n = sequence $ replicate n nextFibo

fibo''' :: Int -> Int
fibo''' 1 = 1
fibo''' 2 = 1
fibo''' n = let (_, (_, nthFibo)) = runFibo in nthFibo
  where
    m = n - 2
    runFibo = runState (buildFiboState m) (1, 1)
