module SimpleChord
( TriadChord
, majorTriad
, minorTriad
, diminishedTriad
, augmentedTriad
) where

import SimplePitch
import SimpleInterval

type TriadChord = (Pitch, Pitch, Pitch)

majorTriad :: Pitch -> TriadChord
majorTriad = getTriad . (sequenceA [id, (Major3rd `above`), (Perfect5th `above`)])
  where
    getTriad [root, third, fifth] = (root, third, fifth)
    
minorTriad :: Pitch -> TriadChord
minorTriad = flat' . majorTriad
  where
    flat' (root, third, fifth) = ( root
                                 , (enharmonicPitch Flat) . (Minor2nd `below`) $ third
                                 , fifth )

diminishedTriad :: Pitch -> TriadChord
diminishedTriad = flat' . majorTriad
  where
    flat' (root, third, fifth) = ( root
                                 , (enharmonicPitch Flat) . (Minor2nd `below`) $ third
                                 , (enharmonicPitch Flat) . (Minor2nd `below`) $ fifth )

augmentedTriad :: Pitch -> TriadChord
augmentedTriad = sharp' . majorTriad
  where
    sharp' (root, third, fifth) = ( root
                                  , third
                                  , (enharmonicPitch Sharp) . (Minor2nd `above`) $ fifth )
