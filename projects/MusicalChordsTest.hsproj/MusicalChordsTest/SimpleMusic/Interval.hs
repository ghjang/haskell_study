{-# LANGUAGE DeriveAnyClass #-}

module SimpleMusic.Interval
( Interval (..)
, above
, below
, diminishInterval
) where

import SimpleMusic.Pitch
import Utility
import Indexable

data Interval = Perfect1st
              | Minor2nd
              | Major2nd
              | Minor3rd
              | Major3rd
              | Perfect4th
              | Augmented4th
              | Perfect5th
              | Minor6th
              | Major6th
              | Minor7th
              | Major7th
              | Perfect8th
      deriving (Eq, Bounded, Enum, Show, Indexable)

aboveM :: Interval -> Pitch -> Maybe Pitch
i `aboveM` (Pitch acc name) = f $ (indexOf i + pn) `mod` 12
  where
    pn = pitchToNum (Pitch acc name)
    f = case acc of Natural -> numToPitchS
                    Flat -> numToPitchF
                    Sharp -> numToPitchS

belowM :: Interval -> Pitch -> Maybe Pitch
i `belowM` p = do
  let n = 12 - indexOf i
  i' <- fromIndex n
  i' `aboveM` p

above :: Interval -> Pitch -> Pitch
i `above` p@(Pitch acc name) = getMaybe $ i `aboveM` p

below :: Interval -> Pitch -> Pitch
i `below` p@(Pitch acc name) = getMaybe $ i `belowM` p

diminishInterval :: Int -> Interval -> Interval
diminishInterval n i = getMaybe . fromIndex $ max (indexOf i - n) 0
