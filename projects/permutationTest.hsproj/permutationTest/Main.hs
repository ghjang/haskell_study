import Data.Monoid

rotateLeft = drop <> take

type SwapListZipper a = ([a], [a], [a])

createSwapListZipper :: [a] -> SwapListZipper a
createSwapListZipper [] = ([], [], [])
createSwapListZipper (x:xs) = ([], [x], xs)

nextSwap :: SwapListZipper a -> SwapListZipper a
nextSwap (ls, [x], []) = (ls, [x], [])
nextSwap (ls, [x], rs) = (ls', [x], rs')
  where
    ls' = [head rs] ++ (rotateLeft 1 ls)
    rs' = drop 1 rs
