module SimpleMusic.SinusoidalSound
( soundSample
, adjustMaxAmplitude
) where

--import Numeric.Limits -- FIXME

soundSample :: Int -> Float -> Float -> (Float -> Float) -> [Float]
soundSample samplingRate frequency duration f
  = take nSample $ map f [0.0, fStep ..]
  where
    samplingRate' = fromIntegral samplingRate
    fStep = (2 * pi * frequency) / samplingRate'
    nSample = floor $ duration * samplingRate'

minMaxPair :: [Float] -> (Float, Float)
--minMaxPair = foldl f (maxValue :: Float, minValue :: Float) -- FIXME
minMaxPair = foldl f (0, 0)
  where
    f (minVal, maxVal) x = (if x < minVal then x else minVal
                           , if x > maxVal then x else maxVal)

adjustMaxAmplitude :: Float -> [Float] -> [Float]
adjustMaxAmplitude newAmplitude samples
  = map (* ratio) samples
  where
    (minVal, maxVal) = minMaxPair samples
    maxVal' = max (abs $ minVal) (abs $ maxVal)
    ratio = newAmplitude / maxVal'