module WeirdNumber
(
) where

import Control.Monad (replicateM_)
import Data.List (subsequences)

properDivisors :: Int -> [Int]
properDivisors n = filter (\x -> n `rem` x == 0) [1..half]
  where half = n `div` 2

hasSemiperfectMatch :: [Int] -> Int -> Bool
hasSemiperfectMatch xs n =
  any (\divisors -> sum divisors == n) $ subsequences xs'
    where xs' = filter (<= n) xs

analyze :: Int -> ([Int], Int, Int, Bool, Bool)
analyze n = (divisors, total, diff, isAbundant, isWeird)
  where
    divisors = properDivisors n
    total = sum divisors
    diff = total - n
    isAbundant = total > n
    ratio = n `div` diff
    isWeird = if not isAbundant || ratio <= 10 then False
              else if even n &&
                      not (hasSemiperfectMatch divisors diff) then True
                                                              else False

analyze1 :: Int -> ([Int], Int, Int, Bool, Int)
analyze1 n = (divisors, total, diff, isAbundant, ratio)
  where
    divisors = properDivisors n
    total = sum divisors
    diff = total - n
    isAbundant = total > n
    ratio = n `div` diff

printWeirdness =
  do
    n <- getLine    
    let (_, _, _, _, isWeird) = analyze (read n)    
    putStrLn $ if isWeird then "weird" else "not weird"

main =
  do
    numOfTest <- getLine
    replicateM_ (read numOfTest) printWeirdness
