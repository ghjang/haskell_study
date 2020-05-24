module Problem_1_1 () where

-- by using a recursion
fact :: (Num a, Eq a) => a -> a
fact 1 = 1
fact n = n * fact (n - 1)

-- by using the 'product' function and list enumeration
fact' :: (Num a, Enum a) => a -> a
fact' n = product [1..n]

-- by using the 'foldl1' function and list enumeration
fact'' :: (Num a, Enum a) => a -> a
fact'' n = foldl1 (*) [1..n]
