{-# LANGUAGE DeriveAnyClass #-}

module SimpleScale
( BasicScale (..)
, SevenMode (..)
, scale
) where

import Data.List
import SimplePitch
import SimpleInterval
import Indexable

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
