module Math.SinusoidalFunction
( naturalHarmonics
, evenHarmonics
, oddHarmonics
, squaredHarmonics
, primeHarmonics
, musicalHarmonics
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


{--
musicalHarmonics :: Float -> Float
musicalHarmonics x = sin x / 1
                        + sin (2 * x) / 2
                        + sin (3 / 2 * x) / 3
                        + sin (4 / 3 * x) / 4
                        -- + sin (5 / 3 * x) / 5
                        -- + sin (5 / 4 * x) / 6
                        -- + sin (8 / 5 * x) / 7
                        -- + sin (6 / 5 * x) / 8
                        --}


musicalHarmonics :: Float -> Float
musicalHarmonics x = sin x / 1
                        + sin (2 * x) / 2
                        -- + sin ((2 ** (1 / 7)) * x) / (2 ** (1 / 7))
                        -- + sin ((2 ** (1 / 5)) * x) / (2 ** (1 / 5))
