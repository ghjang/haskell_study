import System.Random
import System.Time
import Control.Monad.State

currentSec :: IO Int
currentSec = do
  now <- getClockTime
  return . ctSec . toUTCTime $ now

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

randomValues :: (Random a) => Int -> [a]
randomValues seed = let (rs, _) = runRandState in rs
  where
    randState = sequence $ cycle [randomSt]
    runRandState = runState randState (mkStdGen seed)

main = do
  curSec <- currentSec
  let fstFiveInts = take 5 $ (randomValues curSec) :: [Int]
  let fstFiveBools = take 5 $ (randomValues curSec) :: [Bool]
  mapM_ print fstFiveInts
  mapM_ print fstFiveBools
