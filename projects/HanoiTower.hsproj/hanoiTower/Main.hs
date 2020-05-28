import Control.Monad.Writer
import Control.Monad.State


-- moving the Hanoi tower without a Monad
moveDisk :: Int -> Int -> Int -> [(Int, Int, Int)]
moveDisk n from to
  | n == 0 = []
  | otherwise = moveDisk (n - 1) from to'
                  ++
                [(n, from, to)]
                  ++
                moveDisk (n - 1) to' to
  where
    to' = 3 - (from + to)


-- moving the Hanoi tower with IO Monad
moveDisk' :: Int -> Int -> Int -> IO [(Int, Int, Int)]
moveDisk' n from to
  | n == 0 = return []
  | otherwise = do
      prevMov <- moveDisk' (n - 1) from to'
      nextMov <- moveDisk' (n - 1) to' to
      return (prevMov ++ [(n, from, to)] ++ nextMov)
  where
    to' = 3 - (from + to)


-- moving the Hanoi tower with Writer Monad
moveDisk'' :: Int -> Int -> Int -> Writer [(Int, Int, Int)] ()
moveDisk'' n from to
  | n == 0 = return ()
  | otherwise = do
      moveDisk'' (n - 1) from to'
      tell [(n, from, to)]
      moveDisk'' (n - 1) to' to
  where
    to' = 3 - (from + to)
