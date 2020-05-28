import Data.List

data BasePitch = C | D | E | F | G | A | B deriving (Eq,
                                                     Enum,
                                                     Bounded,
                                                     Show)

data Accidental = Natural | Sharp | Flat deriving (Eq, Enum, Bounded)

instance Show Accidental where
  show Natural = ""
  show Sharp = "#"
  show Flat = "b"

newtype Pitch = Pitch (BasePitch, Accidental) deriving Eq

natural :: BasePitch -> Pitch
natural p = Pitch (p, Natural)

sharp :: BasePitch -> Pitch
sharp p = Pitch (p, Sharp)

flat :: BasePitch -> Pitch
flat p = Pitch (p, Flat)

twelvePitchNums = [0..11]

twelvePitchesF :: [Pitch]
twelvePitchesF = (concat $ (zipWith f pitches (repeat Flat)))
                    \\ [flat C, flat F]
  where
    pitches = [(minBound :: BasePitch)..(maxBound :: BasePitch)]
    f = \p f -> [Pitch (p, f), Pitch (p, Natural)]

pitchToNum :: Pitch -> Maybe Int
pitchToNum p = elemIndex p twelvePitchesF

numToPitchF :: Int -> Maybe Pitch
numToPitchF n
  | n >= 0 && n <= 11 = Just . head $ drop n twelvePitchesF
  | otherwise = Nothing

pitchToStr :: Pitch -> String
pitchToStr (Pitch (p, a)) = show p ++ show a
