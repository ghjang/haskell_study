{-# LANGUAGE DeriveAnyClass #-}

module SimpleMusic.Scale
( BasicScale (..)
, SevenMode (..)
, scale
, mode
) where

import Data.List
import SimpleMusic.Pitch
import SimpleMusic.Interval
import Indexable
import Utility

data BasicScale = Major | NaturalMinor | HarmonicMinor | MelodicMinor
      deriving (Eq, Bounded, Enum, Show)
      
data SevenMode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
      deriving (Eq, Bounded, Enum, Show, Indexable)

majorScaleInterval = [Perfect1st, Major2nd, Major3rd, Perfect4th, Perfect5th, Major6th, Major7th]

scaleIntervalToNum :: [Interval] -> [Int]
scaleIntervalToNum = map indexOf

flatScaleInterval :: [Int] -> [Interval] -> [Interval]
flatScaleInterval xs is = map f $ zip [1..] is
  where
    f (x, y) = if x `elem` xs then diminishInterval 1 y else y

scaleInterval :: BasicScale -> [Interval]
scaleInterval s = flatScaleInterval xs majorScaleInterval
  where
    xs = case s of Major -> []
                   NaturalMinor -> [3, 6, 7]
                   HarmonicMinor -> [3, 6]
                   MelodicMinor -> [3]

scale :: BasicScale -> Pitch -> [Pitch]
scale s p = (sequenceA $ map above $ scaleInterval s) p

mode :: SevenMode -> Pitch -> [Pitch]
mode m p = (sequenceA $ map (\x -> (enharmonicPitch Flat) . (x `above`)) $ modeInterval) p'
  where
    modeIndex = indexOf m
    modeInterval = rotateLeft modeIndex majorScaleInterval
    p' = (majorScaleInterval !! modeIndex) `below` p
