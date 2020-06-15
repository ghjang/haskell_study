{-# LANGUAGE BangPatterns #-}

module SimpleMusic.SinusoidalSound
( soundSample
, minMaxPair
, adjustMaxAmplitude
, writeRawPcmWaveDataToFile
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable

--import Numeric.Limits -- FIXME

soundSample :: Int -> Float -> (Float -> Float) -> Float -> Float -> [Float]
soundSample samplingRate duration periodicFunction functionPeriod frequency
  = take nSample $ map periodicFunction [0.0, fStep ..]
  where
    samplingRate' = fromIntegral samplingRate
    fStep = (functionPeriod * frequency) / samplingRate'
    nSample = floor $ duration * samplingRate'

soundSample3 :: Int -> Float -> (Float -> Float) -> Float -> [Float] -> [Float]
soundSample3 samplingRate duration periodicFunction functionPeriod (freq1:freq2:freq3:[]) = []

minMaxPair :: [Float] -> (Float, Float)
--minMaxPair = foldl f (maxValue :: Float, minValue :: Float) -- FIXME
minMaxPair = foldl f (0, 0)
  where
    f (!minVal, !maxVal) x = (if x < minVal then x else minVal
                             , if x > maxVal then x else maxVal)

adjustMaxAmplitude :: Float -> [Float] -> [Float]
adjustMaxAmplitude newAmplitude samples
  = map (* ratio) samples
  where
    (minVal, maxVal) = minMaxPair samples
    maxVal' = max (abs $ minVal) (abs $ maxVal)
    ratio = newAmplitude / maxVal'

writeRawPcmWaveDataToFile :: String -> [Float] -> IO ()
writeRawPcmWaveDataToFile file ws
  = B.writeFile file $ B.toLazyByteString $ fold $ map B.floatLE ws
