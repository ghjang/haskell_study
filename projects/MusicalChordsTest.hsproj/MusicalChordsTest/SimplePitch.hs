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
) where 

import Data.List
import Utility

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

pitchToNum :: Pitch -> Int
pitchToNum (Pitch Natural p) = getMaybe $ elemIndex (Pitch Natural p) twelvePitchF
pitchToNum (Pitch Flat p) = getMaybe $ elemIndex (Pitch Flat p) twelvePitchF 
pitchToNum (Pitch Sharp p) = getMaybe $ elemIndex (Pitch Sharp p) twelvePitchS

numToPitchF :: Int -> Maybe Pitch
numToPitchF n
  | n >= 0 && n <= 11 = Just . head $ drop n twelvePitchF
  | otherwise = Nothing

numToPitchS :: Int -> Maybe Pitch
numToPitchS n
  | n >= 0 && n <= 11 = Just . head $ drop n twelvePitchS
  | otherwise = Nothing

enharmonicPitch :: Accidental -> Pitch -> Pitch
enharmonicPitch Flat (Pitch Sharp E) = Pitch Natural F
enharmonicPitch Flat (Pitch Sharp B) = Pitch Natural C
enharmonicPitch Sharp (Pitch Flat C) = Pitch Natural B
enharmonicPitch Sharp (Pitch Flat F) = Pitch Natural E
enharmonicPitch Flat (Pitch Sharp name) = getMaybe . numToPitchF . pitchToNum $ (Pitch Sharp name)  
enharmonicPitch Sharp (Pitch Flat name) = getMaybe . numToPitchS . pitchToNum $ (Pitch Flat name) 
enharmonicPitch _ p = p
