module SimpleInterval
( Interval (..)
, intervalToNum
) where

import SimplePitch

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
intervalToNum interval = snd $ foldl f 
                                     (False, 0)
                                     [Perfect1st .. Perfect8th]
  where
    f (True, index) _ = (True, index)
    f (False, index) e = if interval == e then (True, index)
                                          else (False, index + 1)

numToInterval :: Int -> Maybe Interval
numToInterval n = foldl f Nothing intervalList
  where
    f (Just interval) _ = Just interval
    f Nothing e = if fst e == n then Just (snd e) else Nothing
    intervalList = zipWith (\x y -> (x, y)) [0..] [Perfect1st .. Perfect8th]

above :: Interval -> Pitch -> Maybe Pitch
i `above` (Pitch acc name) = do
  pn <- pitchToNum (Pitch acc name)
  let f = case acc of Natural -> numToPitchS
                      Flat -> numToPitchF
                      Sharp -> numToPitchS
  f $ (intervalToNum i + pn) `mod` 12

below :: Interval -> Pitch -> Maybe Pitch
i `below` p = do
  let n = 12 - intervalToNum i
  i' <- numToInterval n
  i' `above` p
