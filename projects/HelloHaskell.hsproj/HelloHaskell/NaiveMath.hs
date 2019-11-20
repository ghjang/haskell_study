module NaiveMath
(
) where


gcd' :: Integral a => a -> a -> a

gcd' 0 n = abs n

gcd' m n = let r = n `mod` m
           in gcd' r m
