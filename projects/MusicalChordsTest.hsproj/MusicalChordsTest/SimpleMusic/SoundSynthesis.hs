module SimpleMusic.SoundSynthesis
( noteSoundSample'
, noteSoundSample
) where
  
import SimpleMusic.Pitch
import SimpleMusic.Note
import SimpleMusic.Chord
import SimpleMusic.SinusoidalSound
import Utility

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

noteSoundSample :: Int -> Note -> [Float]
noteSoundSample bpm note = getSamples $ frequencyOf note
  where
    defaultSamplingRate' = fromIntegral defaultSamplingRate
    bpm' = fromIntegral bpm
    duration' = case duration note of
                  Whole -> (60 * 4) / bpm'
                  Half -> (60 * 2) / bpm' 
                  Quater -> (60 * 1) / bpm' 
                  Eighth -> (60 / 2) / bpm'
                  Sixteenth -> (60 / 4) / bpm'
    ----
    getSamples Nothing = replicate (floor $ duration' * defaultSamplingRate') 0.0
    getSamples (Just freq) = soundSample defaultSamplingRate
                                         duration'
                                         defaultPeriodicFunction
                                         defaultFunctionPeriod
                                         freq
