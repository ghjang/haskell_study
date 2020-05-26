-- Fibonacci numbers
fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- '(m, n)' indices
indices :: Int -> Int -> [(Int, Int)]
indices m n = take (m * n) indices'
  where
    indices' :: [(Int, Int)]
    indices' = (1, 1) : map nextIndex indices'

    nextIndex (i, j)
      | j >= n = (i + 1, 1)
      | otherwise = (i, j + 1)

{---------------------------
  ASCII Sierpinski triangles
 ---------------------------}
 
padding :: Int -> Int -> Char -> [[Char]]
padding m n c = replicate m (replicate n c)

nextTri :: [[Char]] -> [[Char]]
nextTri (s:ss) = zipWith3 (\a b c -> a ++ b ++ c) pad1 (s:ss) pad1
                    ++
                 zipWith3 (\a b c -> a ++ b ++ c) (s:ss) pad2 (s:ss)
  where
    h = length (s:ss)
    w = length s
    w' = (w `div` 2) + 1
    pad1 = padding h w' '_'
    pad2 = padding h 1 '_'

sierpinskiTriangles :: [[[Char]]]
sierpinskiTriangles = fstTri : map nextTri sierpinskiTriangles
  where
    fstTri = ["_1_",
              "111"]
