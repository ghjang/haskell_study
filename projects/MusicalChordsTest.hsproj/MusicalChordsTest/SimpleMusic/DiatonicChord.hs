module SimpleMusic.DiatonicChord
( majorScaleDiatonicTriad
) where

import Control.Applicative

import SimpleMusic.Pitch
import SimpleMusic.Chord
import SimpleMusic.Scale
import Tuple
  
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
