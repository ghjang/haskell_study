import SimpleMusic.Pitch
import SimpleMusic.Note
import SimpleMusic.Interval
import SimpleMusic.Scale
import SimpleMusic.Chord
import SimpleMusic.SinusoidalSound
import SimpleMusic.SoundSynthesis
import SimpleMusic.DiatonicChord
import Math.SinusoidalFunction

{--
main = writeRawWaveDataToFile "output.bin" $ (adjustMaxAmplitude 0.75
                                                $ chordSoundSample 4 (BPM Quater 60) Quater
                                                $ minorTriad $ natural D)
                                                  ++
                                             (adjustMaxAmplitude 1.0
                                                $ chordSoundSample 4 (BPM Quater 60) Quater
                                                $ majorTriad $ natural G)
                                                  ++
                                             (adjustMaxAmplitude 0.75
                                                $ chordSoundSample 4 (BPM Quater 60) Quater
                                                $ majorTriad $ natural C)
--}

main = writeRawWaveDataToFile "output.bin" $ concat
                                           $ map f
                                           $ majorScaleDiatonicTriad (scale Major $ natural C)
  where
    f chord = adjustMaxAmplitude 1 $ chordSoundSample 4 (BPM Quater 60) Quater chord

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
