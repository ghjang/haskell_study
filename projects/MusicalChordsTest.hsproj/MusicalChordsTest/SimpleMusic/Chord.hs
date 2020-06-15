module SimpleMusic.Chord
( TriadChord
, majorTriad
, minorTriad
, diminishedTriad
, augmentedTriad
) where

import Tuple
import SimpleMusic.Pitch
import SimpleMusic.Interval

type TriadChord = Triplet Pitch

majorTriad :: Pitch -> TriadChord
majorTriad = getTriad . (sequenceA [id, (Major3rd `above`), (Perfect5th `above`)])
  where
    getTriad [root, third, fifth] = Triplet (root, third, fifth)
    
minorTriad :: Pitch -> TriadChord
minorTriad = flat' . majorTriad
  where
    flat' (Triplet (root, third, fifth))
      = Triplet ( root
                , (enharmonicPitch Flat) . (Minor2nd `below`) $ third
                , fifth )

diminishedTriad :: Pitch -> TriadChord
diminishedTriad = flat' . majorTriad
  where
    flat' (Triplet (root, third, fifth))
      = Triplet ( root
                , (enharmonicPitch Flat) . (Minor2nd `below`) $ third
                , (enharmonicPitch Flat) . (Minor2nd `below`) $ fifth )

augmentedTriad :: Pitch -> TriadChord
augmentedTriad = sharp' . majorTriad
  where
    sharp' (Triplet (root, third, fifth))
       = Triplet ( root
                 , third
                 , (enharmonicPitch Sharp) . (Minor2nd `above`) $ fifth )
