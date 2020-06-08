module SimpleInterval
( Interval (..)
, intervalToNum
, numToInterval
, above
, below
) where

import SimplePitch
import Utility

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
      deriving (Eq, Bounded, Enum, Show)

intervalToNum :: Interval -> Int
intervalToNum interval = fst . head $ dropWhile (\x -> snd x /= interval) l
  where
    l = zipWith (\x y -> (x, y)) [0..] [Perfect1st .. Perfect8th]

numToInterval :: Int -> Maybe Interval
numToInterval n = foldl f Nothing l
  where
    f (Just interval) _ = Just interval
    f Nothing e = if fst e == n then Just (snd e) else Nothing
    l = zipWith (\x y -> (x, y)) [0..] [Perfect1st .. Perfect8th]

aboveM :: Interval -> Pitch -> Maybe Pitch
i `aboveM` (Pitch acc name) = f $ (intervalToNum i + pn) `mod` 12
  where
    pn = pitchToNum (Pitch acc name)
    f = case acc of Natural -> numToPitchS
                    Flat -> numToPitchF
                    Sharp -> numToPitchS

belowM :: Interval -> Pitch -> Maybe Pitch
i `belowM` p = do
  let n = 12 - intervalToNum i
  i' <- numToInterval n
  i' `aboveM` p

above :: Interval -> Pitch -> Pitch
i `above` (Pitch acc name) = getMaybe $ i `aboveM` (Pitch acc name)

below :: Interval -> Pitch -> Pitch
i `below` (Pitch acc name) = getMaybe $ i `belowM` (Pitch acc name)
