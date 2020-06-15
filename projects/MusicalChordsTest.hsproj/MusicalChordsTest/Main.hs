import Control.Applicative

import SimpleMusic.Pitch
import SimpleMusic.Interval
import SimpleMusic.Scale
import SimpleMusic.Chord
import SimpleMusic.SinusoidalSound
import Math.SinusoidalFunction

--main = writeRawWaveDataToFile "output.bin" $ soundSample 44100 220 1 sin

{--
main = writeRawWaveDataToFile "output.bin"
          $ (soundSample 44100 220 0.5 sin)
              ++ (soundSample 44100 440 0.5 sin)
              ++ (soundSample 44100 220 1 sin)
              --}

{--
main = writeRawWaveDataToFile "output.bin"
          $ (soundSample 44100 220 1.5 musicalHarmonics)
              ++ (soundSample 44100 440 1 musicalHarmonics)
              ++ (soundSample 44100 220 1.5 musicalHarmonics)
              --}

main = writeRawWaveDataToFile "output.bin" $ soundSample 44100 220 1 naturalHarmonics

--main = writeRawWaveDataToFile "output.bin" $ soundSample 44100 220 1 evenHarmonics

--main = writeRawWaveDataToFile "output.bin" $ soundSample 44100 220 1 oddHarmonics

--main = writeRawWaveDataToFile "output.bin" $ soundSample 44100 220 1 squaredHarmonics

--main = writeRawWaveDataToFile "output.bin" $ soundSample 44100 220 0.5 squaredHarmonics

--main = writeRawWaveDataToFile "output.bin" $ soundSample 44100 220 1 primeHarmonics


{--
main = mapM_ putStrLn $ zipWith (\x y -> show x
                                           ++ (if hasFlat x || hasSharp x then "\t" else "\t\t")
                                           ++ "Major Scale Diatonic Triads: "
                                           ++ show y)
                                twelvePitchF
                                twelveMajorScaleDiatonicTriad
  where
    twelveMajorScaleDiatonicTriad = map majorScaleDiatonicTriad $ map (scale Major) twelvePitchF
    
--}

type Scale = [Pitch]

majorScaleDiatonicTriad :: Scale -> [TriadChord]
majorScaleDiatonicTriad s = getZipList $ ZipList fs <*> ZipList s
  where
    fs = [ majorTriad
         , minorTriad
         , minorTriad
         , majorTriad
         , majorTriad
         , minorTriad
         , diminishedTriad ]
