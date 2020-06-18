{-# LANGUAGE RecordWildCards #-}

module SimpleMusic.ChordWavePlot
( sceneWithDiatonicTriadChordPlot
) where

import Data.Word (Word16)
import Graphics.SpriteKit

import SimpleMusic.Pitch
import SimpleMusic.Note
import SimpleMusic.Scale
import SimpleMusic.Chord
import SimpleMusic.DiatonicChord
import Math.FunctionPlot
import Tuple
import CircularListZipper

type SceneData = (Maybe TriadChord, CircularListZipper (Maybe TriadChord))
type DiatonicTriadChordPlotScene = Scene SceneData ()

cMajorScaleDiatonicTriads :: [TriadChord]
cMajorScaleDiatonicTriads = majorScaleDiatonicTriad (scale Major $ natural C)

sceneWithDiatonicTriadChordPlot :: DiatonicTriadChordPlotScene
sceneWithDiatonicTriadChordPlot = (sceneWithSize $ Size 1024 768)
                                    { sceneBackgroundColor = whiteColor
                                    , sceneChildren = []
                                    , sceneAnchorPoint = Point 0.5 0.5
                                    , sceneUpdate            = Just updateScene
                                    , sceneHandleEvent       = Just keyEventHandler
                                    , sceneData              = sceneData' }
  where
    sceneData' = next . createCircularListZipper $ map Just cMajorScaleDiatonicTriads

keyEventHandler :: Event -> SceneData -> Maybe SceneData
keyEventHandler (KeyEvent{ keyEventType = KeyDown, keyEventKeyCode = keys}) oldData
  = Just $ case keys of
             0x7B -> prev . snd $ oldData
             0x7C -> next . snd $ oldData
             _ -> oldData
keyEventHandler _ oldData = Just oldData

updateScene :: DiatonicTriadChordPlotScene -> TimeInterval -> DiatonicTriadChordPlotScene
updateScene scene@Scene{..} time
  = case sceneData of
      (Nothing, _) -> scene
      (Just triad, zipper) -> scene { sceneChildren = nodeWithTriadChordPlot triad
                                    , sceneData = (Nothing, zipper) }

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
