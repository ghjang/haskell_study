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
import Utility
import CircularListZipper

data Zoom = ZoomIn | ZoomOut

data SceneData = SceneData { lastChord :: Maybe TriadChord
                           , zoomAction :: Maybe Zoom
                           , plotRange :: (GFloat, GFloat)
                           , iterator :: (Maybe TriadChord, CircularListZipper (Maybe TriadChord)) }
      
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
    sceneData' = SceneData { lastChord = Nothing
                           , zoomAction = Nothing
                           , plotRange = (negate 0.025, 0.025)
                           , iterator = next . createCircularListZipper $ map Just cMajorScaleDiatonicTriads }

keyEventHandler :: Event -> SceneData -> Maybe SceneData

keyEventHandler (KeyEvent{ keyEventType = KeyDown, keyEventKeyCode = keys}) oldData
  = case keys of
      0x7E -> Just $ oldData { zoomAction = Just ZoomIn }                     -- up arrow
      0x7D -> Just $ oldData { zoomAction = Just ZoomOut }                    -- down arrow
      0x7B -> Just $ oldData { iterator = prev . snd . iterator $ oldData }   -- left arrow
      0x7C -> Just $ oldData { iterator = next . snd . iterator $ oldData }   -- right arrow
      _ -> Nothing

keyEventHandler _ _ = Nothing

updateScene :: DiatonicTriadChordPlotScene -> TimeInterval -> DiatonicTriadChordPlotScene
updateScene scene@Scene{..} time
  = case iterator sceneData of
      (Nothing, _) -> case zoomAction sceneData of
                        Nothing -> scene
                        Just ZoomIn -> zoomInScene sceneData
                        Just ZoomOut -> zoomOutScene sceneData
      (Just triad, zipper) -> scene { sceneChildren = nodeWithTriadChordPlot triad (plotRange sceneData)
                                    , sceneData = sceneData { lastChord = Just triad
                                                            , iterator = (Nothing, zipper) } }
  where
    zoomPlot triad plotRange sData = scene { sceneChildren = nodeWithTriadChordPlot triad plotRange
                                           , sceneData = sData { plotRange = plotRange
                                                               , zoomAction = Nothing } }
    zoomInScene sData@SceneData{..}
      = let plotRange' = ( min (negate 0.005) (fst plotRange + 0.005)
                         , max 0.005 (snd plotRange - 0.005) )
        in zoomPlot (getMaybe $ lastChord) plotRange' sData
    zoomOutScene sData@SceneData{..}
      = let plotRange' = (fst plotRange - 0.005, snd plotRange + 0.005)
        in zoomPlot (getMaybe $ lastChord) plotRange' sData

nodeWithTriadChordPlot :: TriadChord -> (GFloat, GFloat) -> [Node ()]
nodeWithTriadChordPlot triad fRange = [ (plot (Size 1024 768) fRange ((75 *) . f))
                                      , labelWithText (show . getTriplet $ triad) (Point (negate 256) 300)]
  where
    (Just rootFreq, Just thirdFreq, Just fifthFreq)
        = getTriplet $ fmap frequencyOf
                     $ triadChordNote 4 triad Quater
    (rf:tf:ff:[]) = map realToFrac [rootFreq, thirdFreq, fifthFreq]
    ----
    f t = sum $ map (sin . (\f -> 2 * pi * f * t)) [rf, tf, ff]
    ----
    labelWithText txt point = (labelNodeWithFontNamed "Helvetica Neue Bold")
                                { labelText = txt
                                , labelFontSize = 55
                                , labelFontColor = darkGrayColor
                                , nodePosition = point }
