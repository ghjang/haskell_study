import Control.Concurrent.Async

import SimpleMusic.Pitch
import SimpleMusic.Note
import SimpleMusic.Scale
import SimpleMusic.DiatonicChord
import SimpleMusic.SinusoidalSound
import SimpleMusic.SoundSynthesis

main = do
  let triads = majorScaleDiatonicTriad (scale Major $ natural C)
  sss <- mapConcurrently f triads
  writeRawPcmWaveDataToFile "output.bin" $ concat sss
  ----
  where
    f chord = do
      ss <- chordSoundSampleAsync 4 (BPM Quater 60) Quater chord
      return $ adjustMaxAmplitude 1 ss
