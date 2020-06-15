{-# LANGUAGE ParallelListComp #-}

module SimpleMusic.SoundSynthesis
( BPM (..)
, noteSoundSample'
, noteSoundSample
, chordSoundSample
) where
  
import SimpleMusic.Pitch
import SimpleMusic.Note
import SimpleMusic.Chord
import SimpleMusic.SinusoidalSound
import Utility
import Tuple

data BPM = BPM Duration Int

defaultSamplingRate = 44100 :: Int

defaultPeriodicFunction = sin
defaultFunctionPeriod = 2 * pi

noteSoundSample' :: Float -> Note -> [Float]
noteSoundSample' duration note = getSamples $ frequencyOf note
  where
    defaultSamplingRate' = fromIntegral defaultSamplingRate
    getSamples Nothing = replicate (floor $ duration * defaultSamplingRate') 0.0
    getSamples (Just freq) = soundSample defaultSamplingRate
                                         duration
                                         defaultPeriodicFunction
                                         defaultFunctionPeriod
                                         freq

noteSoundSample :: BPM -> Note -> [Float]
noteSoundSample (BPM Quater bpm) note = getSamples $ frequencyOf note
  where
    defaultSamplingRate' = fromIntegral defaultSamplingRate
    bpm' = fromIntegral bpm
    duration' = case duration note of
                  Whole     -> (60 / bpm') * 4
                  Half      -> (60 / bpm') * 2
                  Quater    -> (60 / bpm') * 1 
                  Eighth    -> (60 / bpm') / 2
                  Sixteenth -> (60 / bpm') / 4
    ----
    getSamples Nothing = replicate (floor $ duration' * defaultSamplingRate') 0.0
    getSamples (Just freq) = soundSample defaultSamplingRate
                                         duration'
                                         defaultPeriodicFunction
                                         defaultFunctionPeriod
                                         freq

chordSoundSample :: Octave -> BPM -> Duration -> TriadChord -> [Float]
chordSoundSample octave bpm noteDuration triad
  = [ (r + t + f)
    | r <- noteSoundSample bpm root
    | t <- {-- adjustSamples 0.75 $ --} noteSoundSample bpm third
    | f <- {-- adjustSamples 0.5 $ --} noteSoundSample bpm fifth ]
  where
    (root, third, fifth) = getTriplet $ triadChordNote octave triad noteDuration
    ----
    adjustSamples ratio samples = let (_, maxVal) = minMaxPair samples
                                  in adjustMaxAmplitude (maxVal * ratio) samples
