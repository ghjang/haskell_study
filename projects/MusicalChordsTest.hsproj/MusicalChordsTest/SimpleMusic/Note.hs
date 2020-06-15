{-# LANGUAGE NamedFieldPuns #-}

module SimpleMusic.Note
( Octave
, Duration (..)
, Note (..)
, note
, midiNumberOf
, noteFromMidiNumber
, frequencyOf
, triadChordNote
) where

import SimpleMusic.Pitch
import SimpleMusic.Chord
import Utility
import Tuple hiding (rotateLeft)

type Octave = Int

data Duration = Whole | Half | Quater | Eighth | Sixteenth
      deriving (Show)

data Note = Note { octave   :: Octave
                 , pitch    :: Maybe Pitch
                 , duration :: Duration }
      deriving (Show)

note = Note { octave = 4, pitch = Nothing, duration = Quater }

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
                  octave = (n `div` 12) - 1
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

triadChordNote :: Int -> TriadChord -> Duration -> Triplet Note
triadChordNote rootOctave triad@(Triplet (root, third, fifth)) duration' = fmap toNote triad
  where
    toNote pitch@(Pitch acc name)
      = if pitch == root then note { octave = rootOctave, pitch = Just root, duration = duration' }
                         else note { octave = adjustOctave root pitch, pitch = Just pitch, duration = duration' }
    ----
    adjustOctave p1 p2 = let diff = (pitchToNum p2) - (pitchToNum p1)
                         in if diff < 0 then rootOctave + 1
                                        else rootOctave
                          