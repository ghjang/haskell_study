{-# LANGUAGE RecordWildCards #-}

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
    rightBottom = Point ((pointX $ rectOrigin br) + (sizeWidth $ rectSize br))
                        ((pointY $ rectOrigin br) - (sizeHeight $ rectSize br))
    leftBottom = Point (pointX $ rectOrigin br)
                       ((pointY $ rectOrigin br) - (sizeHeight $ rectSize br))

addBorder :: Color -> GFloat -> Node u -> Node u
addBorder color width n@Sprite{..} = node [ n, borderShape ]
  where
    borderShape = (shapeNodeWithPath
                    [ MoveToPoint leftTop
                    , AddLineToPoint rightTop
                    , AddLineToPoint rightBottom
                    , AddLineToPoint leftBottom
                    , CloseSubpath ])
                    { shapeStrokeColor = color
                    , shapeLineWidth = width }
    (leftTop, rightTop, rightBottom, leftBottom) = boundingRect' n
