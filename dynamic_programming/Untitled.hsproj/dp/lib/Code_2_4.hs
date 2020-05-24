module Code_2_4 () where

-- by using top-down recursion
fibo :: Int -> Int
fibo 1 = 1
fibo 2 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

-- by using a bottom-up approach with the 'foldl' function
fibo' :: Int -> Int
fibo' 1 = 1
fibo' 2 = 1
fibo' n = head $ foldl f [1,1] [1..(n - 2)]
  where
    f (a:b:fs) _ = (a + b):(a:b:[])
