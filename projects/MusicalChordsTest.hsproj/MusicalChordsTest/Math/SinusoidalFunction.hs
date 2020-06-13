module Math.SinusoidalFunction
( naturalHarmonics
, evenHarmonics
, oddHarmonics
, squaredHarmonics
, primeHarmonics
) where

import Math.PrimeNumber (primes)

naturalNums = [2..]
evenNums    = [2,4..]
oddNums     = [3,5..]
squaredNums = map (\x -> x * x) [2..]

harmonicTerm :: Float -> (Float -> Float)
harmonicTerm m = (/ m) . sin . (m *)

takeHarmonicTerm :: Int -> [Float] -> [(Float -> Float)]
takeHarmonicTerm n ms = take n $ map harmonicTerm (1:ms)

harmonicTermSum :: [Float] -> Float -> Float
harmonicTermSum ms = sum . (sequenceA $ takeHarmonicTerm 12 ms)


naturalHarmonics :: Float -> Float
naturalHarmonics = harmonicTermSum naturalNums

evenHarmonics :: Float -> Float
evenHarmonics = harmonicTermSum evenNums

oddHarmonics :: Float -> Float
oddHarmonics = harmonicTermSum oddNums

squaredHarmonics :: Float -> Float
squaredHarmonics = harmonicTermSum squaredNums

primeHarmonics :: Float -> Float
primeHarmonics = harmonicTermSum (map fromIntegral primes)
