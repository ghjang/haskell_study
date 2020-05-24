module Code_1_10 () where

-- by using a recursion
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = let (m, ys) = extractFstMaxVal xs
                in (bubbleSort ys) ++ [m]

{-
  It returns a pair comprised of the max value of the list
  and the remaining list where the first max value is removed from
  the original list.
 -}
extractFstMaxVal :: (Ord a) => [a] -> (a, [a])
extractFstMaxVal (x:xs) = foldl f (x, []) xs
  where
    f (m, ys) y = if y > m then (y, m:ys)
                           else (m, y:ys)
