import Data.List
import Data.Monoid
import Control.Monad.State


rotateLeft = drop <> take


type SwapListZipper a = ([a], [a], [a])

createSwapListZipper :: [a] -> SwapListZipper a
createSwapListZipper (x:xs) = ([], [x], xs)

nextSwap :: SwapListZipper a -> SwapListZipper a
nextSwap (ls, [x], []) = (ls, [x], [])
nextSwap (ls, [x], rs) = (ls', [x], rs')
  where
    ls' = [head rs] ++ (rotateLeft 1 ls)
    rs' = drop 1 rs

curList :: SwapListZipper a -> [a]
curList (ls, x, rs) = concat[ls, x, rs]


headElemSwapList :: [a] -> [[a]]
headElemSwapList xs = let (sss, _) = runSwap in sss
  where
    runSwap = runState swapState (createSwapListZipper xs)
    swapState = sequence $ replicate (length xs) nextSwapState
    
    nextSwapState :: State (SwapListZipper a) [a]
    nextSwapState = state $ \zipper -> (curList zipper, nextSwap zipper)


perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat $ zipWith merge curSwapList subPermList
  where
    curSwapList = headElemSwapList (x:xs)
    subPermList = map (\(_:ys) -> perms ys) $ curSwapList
    
    merge (x:xs) yss = map (x:) yss
