module NaiveMath
(
) where


-- GCD
gcd' :: Integral a => a -> a -> a

gcd' 0 n = abs n

gcd' m n = let r = n `mod` m
           in gcd' r m


-- Factorial
factorial :: Integral a => a -> a

factorial 0 = 1

factorial n = product [1..n]


-- Permutation
nPr :: Integral a => a -> a -> a

nPr n 0 = 1

nPr n r
  | n < 0 || r < 0 = error "n or r should not have a negative value."
  | n < r = error "n should be greater than or equal to r."
  | n == r = factorial n
  | otherwise = product [n, (n - 1) .. (n - r + 1)]
