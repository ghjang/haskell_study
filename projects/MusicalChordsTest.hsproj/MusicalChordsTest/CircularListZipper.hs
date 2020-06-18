module CircularListZipper
( CircularListZipper
, next
, prev
) where
  
type CircularListZipper a = (([a], [a]), ([a], [a]))

createCircularListZipper :: [a] -> CircularListZipper a
createCircularListZipper [] = error "empty list is passed."
createCircularListZipper xs = (([], []), (xs, reverse xs))

next :: CircularListZipper a -> (a, CircularListZipper a)
next (([], []), ([], [])) = error "empty list"
next ((xs, ys), ([], [])) = next (([], []), (ys, xs))
next ((xs, ys), (f:fs, b:bs)) = (f, ((f:xs, b:ys), (fs, bs)))

prev :: CircularListZipper a -> (a, CircularListZipper a)
prev (([], []), ([], [])) = error "empty list"
prev ((xs, ys), ([], [])) = prev (([], []), (ys, xs))
prev ((xs, ys), (f:fs, b:bs)) = (b, ((f:xs, b:ys), (fs, bs)))
