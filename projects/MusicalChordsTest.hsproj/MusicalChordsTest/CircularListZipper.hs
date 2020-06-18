module CircularListZipper
( CircularListZipper
, createCircularListZipper
, next
, prev
, nextItem
, prevItem
) where
  
import Control.Monad.State

data Direction = Forward | Backward deriving Show
  
type CircularListZipper a = (([a], [a]), ([a], [a]), Maybe Direction)

createCircularListZipper :: [a] -> CircularListZipper a
createCircularListZipper [] = error "empty list is passed."
createCircularListZipper xs = (([], []), (xs, reverse xs), Nothing)

next :: CircularListZipper a -> (a, CircularListZipper a)
next (([], []), ([], []), _) = error "empty list"
next ((xs, ys), ([], []), Just Backward) = next . snd $ next (([], []), (ys, xs), Just Forward)
next ((xs, ys), ([], []), _) = next (([], []), (ys, xs), Just Forward)
next ((xs, ys), (f:fs, b:bs), _) = (f, ((f:xs, b:ys), (fs, bs), Just Forward))

prev :: CircularListZipper a -> (a, CircularListZipper a)
prev (([], []), ([], []), _) = error "empty list"
prev ((xs, ys), ([], []), Just Forward) = prev . snd $ prev (([], []), (ys, xs), Just Backward)
prev ((xs, ys), ([], []), _) = prev (([], []), (ys, xs), Just Backward)
prev ((xs, ys), (f:fs, b:bs), _) = (b, ((f:xs, b:ys), (fs, bs), Just Backward))

nextItem :: State (CircularListZipper a) a
nextItem = state next

prevItem :: State (CircularListZipper a) a
prevItem = state prev
