module Chord
( TriadChord
, majorTriad
, minorTriad
, diminishedTriad
, augmentedTriad
) where

import SimplePitch
import SimpleInterval

type TriadChord = (Pitch, Pitch, Pitch)

getTriad :: [Maybe Pitch] -> TriadChord
getTriad [Just root, Just third, Just fifth] = (root, third, fifth)

majorTriad :: Pitch -> TriadChord
majorTriad root = getTriad $ f root
  where
    f = sequenceA [\_ -> Just root,
                   (Major3rd `above`),
                   (Perfect5th `above`)]

minorTriad :: Pitch -> TriadChord
minorTriad root = getTriad $ f root
  where
    f = sequenceA [\_ -> Just root,
                   (enharmonicPitchM Flat) . (Minor3rd `above`),
                   (Perfect5th `above`)]

diminishedTriad :: Pitch -> TriadChord
diminishedTriad root = getTriad $ f root
  where
    f = sequenceA [\_ -> Just root,
                   (enharmonicPitchM Flat) . (Minor3rd `above`),
                   (enharmonicPitchM Flat) . (Augmented4th `above`)]

augmentedTriad :: Pitch -> TriadChord
augmentedTriad root = getTriad $ f root
  where
    f = sequenceA [\_ -> Just root,
                   (Major3rd `above`),
                   (enharmonicPitchM Sharp) . (Minor6th `above`)]
