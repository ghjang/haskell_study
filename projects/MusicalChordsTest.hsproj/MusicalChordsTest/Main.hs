import Control.Applicative
import SimplePitch
import SimpleInterval
import SimpleScale
import Chord

main = mapM_ putStrLn $ zipWith (\x y -> show x
                                           ++ (if hasFlat x || hasSharp x then "\t" else "\t\t")
                                           ++ "Major Scale Diatonic Triads: "
                                           ++ show y)
                                twelvePitchF
                                twelveMajorScaleDiatonicTriad
  where
    twelveMajorScaleDiatonicTriad = map majorScaleDiatonicTriad $ map (scale Major) twelvePitchF

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
