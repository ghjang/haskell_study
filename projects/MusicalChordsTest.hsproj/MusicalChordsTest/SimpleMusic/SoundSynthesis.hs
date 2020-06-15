module SimpleMusic.SoundSynthesis
( BPM (..)
, noteSoundSample'
, noteSoundSample
) where
  
import SimpleMusic.Pitch
import SimpleMusic.Note
import SimpleMusic.Chord
import SimpleMusic.SinusoidalSound
import Utility

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
