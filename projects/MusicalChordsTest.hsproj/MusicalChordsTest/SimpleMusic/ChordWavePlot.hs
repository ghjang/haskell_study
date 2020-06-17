module SimpleMusic.ChordWavePlot
( sceneWithTriadChordPlot
) where

import Graphics.SpriteKit

import SimpleMusic.Pitch
import SimpleMusic.Note
import SimpleMusic.Chord
import Math.FunctionPlot
import Tuple

sceneWithTriadChordPlot :: TriadChord -> Scene () ()
sceneWithTriadChordPlot triad = (sceneWithSize $ Size 1024 768)
                                    { sceneChildren = nodeWithTriadChordPlot triad
                                    , sceneAnchorPoint = Point 0.5 0.5 }

nodeWithTriadChordPlot :: TriadChord -> [Node ()]
nodeWithTriadChordPlot triad = [ (plot (Size 1024 768) fRange ((75 *) . f))
                               , labelWithText (show . getTriplet $ triad) (Point (negate 256) 300)]
  where
    (Just rootFreq, Just thirdFreq, Just fifthFreq)
        = getTriplet $ fmap frequencyOf
                     $ triadChordNote 4 triad Quater
    (rf:tf:ff:[]) = map realToFrac [rootFreq, thirdFreq, fifthFreq]
    ----
    fRange = (negate 0.025, 0.025)
    ----
    f t = sum $ map (sin . (\f -> 2 * pi * f * t)) [rf, tf, ff]
    ----
    labelWithText txt point = (labelNodeWithFontNamed "Helvetica Neue Bold")
                                { labelText = txt
                                , labelFontSize = 55
                                , labelFontColor = darkGrayColor
                                , nodePosition = point }
