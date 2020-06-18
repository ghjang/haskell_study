module CircularListZipper
( CircularListZipper
, createCircularListZipper
, next
, prev
, nextItem
, prevItem
) where
  
import Control.Monad.State

data CircularListZipper a = Forward ([a], [a], [a]) | Backward ([a], [a], [a]) deriving Show

createCircularListZipper :: [a] -> CircularListZipper a
createCircularListZipper xs = Forward (xs, [], xs)

next :: CircularListZipper a -> (a, CircularListZipper a)
next (Forward  (xs, ys,   []))   = next $ Forward (xs, ys, xs)
next (Forward  (xs, ys,   z:zs)) = (z, Forward (xs, z:ys, zs))
next (Backward (xs, [],   z:zs)) = next $ Forward (xs, [z], zs)
next (Backward (xs, ys,   []))   = next $ Forward (xs, ys, xs)
next (Backward (xs, y:ys, zs))   = next . snd . next . snd . next $ Forward (xs, ys, y:zs)

prev :: CircularListZipper a -> (a, CircularListZipper a)
prev (Forward  (xs, [],   zs))   = prev $ Backward (xs, reverse xs, zs)
prev (Forward  (xs, y:ys, []))   = prev $ Backward (xs, ys, [y])
prev (Forward  (xs, ys,   z:zs)) = prev . snd . prev . snd . prev $ Backward (xs, z:ys, zs)
prev (Backward (xs, [],   zs))   = prev $ Backward (xs, reverse xs, zs)
prev (Backward (xs, y:ys, zs))   = (y, Backward (xs, ys, y:zs))

nextItem :: State (CircularListZipper a) a
nextItem = state next

prevItem :: State (CircularListZipper a) a
prevItem = state prev
