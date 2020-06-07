module SimplePitch
( BasePitchName (..)
, Accidental (..)
, Pitch (Pitch)
, natural
, flat
, sharp
, hasFlat
, hasSharp
, twelvePitchF
, twelvePitchS
, pitchToNum
, numToPitchF
, numToPitchS
, enharmonicPitch
, enharmonicPitchM
) where 

import Data.List

data BasePitchName = C | D | E | F | G | A | B
      deriving (Eq, Enum, Bounded, Show)

data Accidental = Natural | Flat | Sharp
      deriving (Eq, Enum, Bounded)

instance Show Accidental where
  show Natural = ""
  show Flat = "b"
  show Sharp = "#"

data Pitch = Pitch Accidental BasePitchName deriving Eq

instance Show Pitch where
  show (Pitch a b) = show b ++ show a

natural :: BasePitchName -> Pitch
natural = Pitch Natural

flat :: BasePitchName -> Pitch
flat = Pitch Flat

sharp :: BasePitchName -> Pitch
sharp = Pitch Sharp

hasFlat :: Pitch -> Bool
hasFlat (Pitch Flat _) = True
hasFlat _ = False

hasSharp :: Pitch -> Bool
hasSharp (Pitch Sharp _) = True
hasSharp _ = False

twelvePitchNum = [0..11]

twelvePitchF :: [Pitch]
twelvePitchF = (concat $ (zipWith f pitches (repeat Flat)))
                    \\ [flat C, flat F]
  where
    pitches = [(minBound :: BasePitchName)..(maxBound :: BasePitchName)]
    f = \p f -> [Pitch f p, Pitch Natural p]
    
twelvePitchS :: [Pitch]
twelvePitchS = (concat $ (zipWith f pitches (repeat Sharp)))
                    \\ [sharp E, sharp B]
  where
    pitches = [(minBound :: BasePitchName)..(maxBound :: BasePitchName)]
    f = \p s -> [Pitch Natural p, Pitch s p]

pitchToNum :: Pitch -> Maybe Int
pitchToNum (Pitch Natural p) = elemIndex (Pitch Natural p) twelvePitchF
pitchToNum (Pitch Flat p) = elemIndex (Pitch Flat p) twelvePitchF 
pitchToNum (Pitch Sharp p) = elemIndex (Pitch Sharp p) twelvePitchS

numToPitchF :: Int -> Maybe Pitch
numToPitchF n
  | n >= 0 && n <= 11 = Just . head $ drop n twelvePitchF
  | otherwise = Nothing

numToPitchS :: Int -> Maybe Pitch
numToPitchS n
  | n >= 0 && n <= 11 = Just . head $ drop n twelvePitchS
  | otherwise = Nothing

enharmonicPitch :: Accidental -> Pitch -> Maybe Pitch
enharmonicPitch Flat (Pitch Sharp E) = Just (Pitch Natural F)
enharmonicPitch Flat (Pitch Sharp B) = Just (Pitch Natural C)
enharmonicPitch Sharp (Pitch Flat C) = Just (Pitch Natural B)
enharmonicPitch Sharp (Pitch Flat F) = Just (Pitch Natural E)
enharmonicPitch Flat (Pitch Sharp name) = do
  n <- pitchToNum (Pitch Sharp name)
  numToPitchF n
enharmonicPitch Sharp (Pitch Flat name) = do
  n <- pitchToNum (Pitch Flat name)
  numToPitchS n
enharmonicPitch _ p = Just p

enharmonicPitchM :: Accidental -> Maybe Pitch -> Maybe Pitch
enharmonicPitchM acc (Just p) = enharmonicPitch acc p
enharmonicPitchM _ Nothing = Nothing
