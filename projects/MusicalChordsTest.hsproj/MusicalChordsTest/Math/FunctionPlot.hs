{-# LANGUAGE RecordWildCards,
             ParallelListComp #-}

module Math.FunctionPlot
( plot
) where

import Graphics.SpriteKit

box :: Color -> Point -> Size -> Node ()
box color position size = (spriteWithColorSize color size)
                              { nodePosition = position }

boundingRect :: Node u -> Rect
boundingRect n@Sprite{..} = Rect { rectOrigin = leftTop
                                 , rectSize = spriteSize }
  where
    sw = sizeWidth spriteSize
    sh = sizeHeight spriteSize
    leftTop = addNodePosition $ Point (negate (sw / 2)) (sh / 2)
    addNodePosition pt = Point (pointX nodePosition + pointX pt)
                               (pointY nodePosition + pointY pt)

boundingRect' :: Node u -> (Point, Point, Point, Point)
boundingRect' n@Sprite{..} = (leftTop, rightTop, rightBottom, leftBottom)
  where
    br = boundingRect n
    leftTop = rectOrigin br
    rightTop = Point ((pointX $ rectOrigin br) + (sizeWidth $ rectSize br))
                     (pointY $ rectOrigin br )
    rightBottom = Point (pointX rightTop)
                        ((pointY $ rectOrigin br) - (sizeHeight $ rectSize br))
    leftBottom = Point (pointX $ rectOrigin br)
                       (pointY rightBottom)

borderShape :: Color -> GFloat -> Node u -> Node u
borderShape color width n@Sprite{..} = (shapeNodeWithPath
                                          [ MoveToPoint leftTop
                                          , AddLineToPoint rightTop
                                          , AddLineToPoint rightBottom
                                          , AddLineToPoint leftBottom
                                          , CloseSubpath ])
                                          { shapeStrokeColor = color
                                          , shapeLineWidth = width }
  where
    (leftTop, rightTop, rightBottom, leftBottom) = boundingRect' n

axisShape :: Color -> GFloat -> Node u -> Node u
axisShape color width n@Sprite{..} = (shapeNodeWithPath
                                       [ MoveToPoint centerTop
                                       , AddLineToPoint centerBottom
                                       , MoveToPoint middleLeft
                                       , AddLineToPoint middleRight
                                       , CloseSubpath ])
                                       { shapeStrokeColor = color
                                       , shapeLineWidth = width }
  where
    br = boundingRect n
    centerTop = Point ((pointX $ rectOrigin br) + ((sizeWidth $ rectSize br) / 2))
                      (pointY $ rectOrigin br)
    centerBottom = Point (pointX centerTop)
                         ((pointY $ rectOrigin br) - (sizeHeight $ rectSize br))
    middleLeft = Point (pointX $ rectOrigin br)
                       ((pointY $ rectOrigin br) - ((sizeHeight $ rectSize br) / 2))
    middleRight = Point ((pointX $ rectOrigin br) + (sizeWidth $ rectSize br))
                        (pointY middleLeft)

functionPathShape :: Color -> GFloat
                        -> GFloat -> GFloat
                        -> (GFloat, GFloat) -> (GFloat -> GFloat)
                        -> Node u
functionPathShape lineColor lineWidth screenWidth yOffset fDomain@(from, to) f
  = (shapeNodeWithPath
        $ [MoveToPoint $ Point xOffset ((f from) + yOffset)]
             ++
          tail fPath)
        { shapeStrokeColor = lineColor
        , shapeLineWidth = lineWidth }
  where
    xOffset = negate $ screenWidth / 2
    domainWidth = abs $ to - from
    fStep = domainWidth / screenWidth
    fPath = [ AddLineToPoint $ Point (x + xOffset) ((f x') + yOffset)
            | x <- [0..]
            | x' <- [from, from + fStep .. to] ]

plot :: Size -> (GFloat, GFloat) -> (GFloat -> GFloat) -> Node ()
plot outputSize (from, to) f
  = node $ [whiteBgBox]
              ++
           (sequenceA [ axisShape blackColor 1
                      , borderShape darkGrayColor 1.75]) whiteBgBox
              ++
           [functionPathShape redColor
                              1
                              (sizeWidth $ spriteSize whiteBgBox)
                              0
                              (from, to)
                              f]
  where
    whiteBgBox = box whiteColor pointZero outputSize
