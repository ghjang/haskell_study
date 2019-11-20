module NaiveMath
(
) where


gcd' :: Integral a => a -> a -> a

gcd' 0 n = n

gcd' m n = let r = n `mod` m
           in case r of 0 -> abs m
                        _ -> gcd' r m
