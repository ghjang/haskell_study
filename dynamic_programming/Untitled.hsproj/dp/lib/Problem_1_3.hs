module Problem_1_3 () where

-- by using a recursion
gugudan :: Int -> [(Int, Int, Int)]
gugudan n = mul 1 1
  where
    mul x y
      | (x > n) = []
      | (y > n) = mul (x + 1) 1
      | otherwise = (x, y, x * y) : mul x (y + 1)

-- by using the list monad's non-determinism
gugudan' :: Int -> [(Int, Int, Int)]
gugudan' n = map mul $ sequence [[1..n], [1..n]]
  where
    mul = \(x:y:[]) -> (x, y, x * y)
