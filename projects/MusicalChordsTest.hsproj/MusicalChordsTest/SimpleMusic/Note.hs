{-# LANGUAGE NamedFieldPuns #-}

module SimpleMusic.Note
( Octave
, Duration
, Note
, note
, midiNumberOf
, noteFromMidiNumber
, frequencyOf
) where

import SimpleMusic.Pitch
import Utility

type Octave = Int

data Duration = Whole | Half | Quater | Eighth | Sixteenth
      deriving (Show)

data Note = Note { octave   :: Octave
                 , pitch    :: Maybe Pitch
                 , duration :: Duration }
      deriving (Show)

note = Note { octave = 4, pitch = Nothing, duration = Whole }

a4 = note { pitch = Just $ natural A }

midiNumberOf :: Note -> Maybe Int
midiNumberOf Note { pitch = Nothing } = Nothing
midiNumberOf Note { octave, pitch = Just pitch' }
  = if midiNumber < 0 || midiNumber > 127 then Nothing
                                          else Just midiNumber
  where
    midiNumber = (octave * 12) + (pitchToNum pitch') + 12

noteFromMidiNumber :: Int -> Maybe Note
noteFromMidiNumber n
  | n < 0 || n > 127 = Nothing
  | otherwise = let
                  (q, r) = (n `div` 12, n `mod` 12)
                  octave = q - 1
                  pitch = numToPitch $ n - (12 * octave) - 12
                in
                  Just $ note { octave = octave
                              , pitch = pitch }

frequencyOf :: Note -> Maybe Float
frequencyOf note = case midiNumberOf note of
                      Nothing -> Nothing
                      Just n -> calc n
  where
    midiNumOfA4 = getMaybe $ midiNumberOf a4
    freqOfA4 = 440
    ----
    calc n = let
                diff = n - midiNumOfA4
                ratio = 2 ** (fromIntegral diff / 12)
             in
                Just (freqOfA4 * ratio)
