{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | The views for the TODO app
module LambdaSVG where

import React.Flux
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.JSString as JSS



import LambdaAST
import LambdaFBAnn
--import LambdaDispatcher
import LambdaStore


renderExprSVG :: Expr FBAnn -> Float -> ReactElementM 'EventHandlerCode ()
renderExprSVG expr _factor =
  let
    expr' = layoutExpr (0,yoffset+10) expr
    ann = getAnn expr'
    expr'' = shiftX 0 expr'
    width :: Int
    width = caW ann + 10
    scaledWidth :: Int
    scaledWidth = round (_factor * fromIntegral (caW ann + 10))
    height :: Int
    height = caH ann + 20
    scaledHeight :: Int
    scaledHeight = round (_factor * fromIntegral (caH ann + 20))
  in
    div_
      [ style
        [ ("position", "relative")
        , ("width", JSS.pack (show scaledWidth ++ "px"))
        , ("height", JSS.pack (show scaledHeight ++ "px"))
        , ("overflow", "hidden")
        --, ("transform", JSS.pack ("scale(" ++ show _factor ++ "," ++ show _factor ++ ")"))
        ]
      ] $
      svg_
        [ "width" &= (show width)
        , "height" &= (show height)
        , "viewBox" &= ("0 0 " ++ show width ++ " " ++ show height)
        --, "transform" &= ("scale(" ++ show _factor ++ "," ++ show _factor ++ ")")
        --, "transform-origin" &= (show (div width 2) ++ "px 0px")
        , style
            [ ("transform", JSS.pack ("scale(" ++ show _factor ++ "," ++ show _factor ++ ")"))
            , ("transformOrigin", "0 0")
            --, ("transform-origin", JSS.pack (show (div width 2) ++ "px 0px"))
--            , ("transform", JSS.pack ("translateX(-" ++ show (div width 2) ++ ")"))
            ]
        ]
        (
          drawExpr [] expr''
        )


drawLine :: (Int, Int) -> (Int, Int) -> ReactElementM 'EventHandlerCode ()
drawLine (x1,y1) (x2,y2) =
  line_
    [ "x1" &= show x1
    , "y1" &= show y1
    , "x2" &= show x2
    , "y2" &= show y2
    , "stroke" &= ("grey" :: String)
    , "strokeWidth" &= ("2" :: String)
    ] $ mempty


data CoordAnn = CoordAnn
  { caX :: Int
  , caY :: Int
  , caW :: Int
  , caH :: Int
  , caXOff :: Int
  , caFBAnn :: FBAnn
  }


shiftXCoordAnn :: Int -> CoordAnn -> CoordAnn
shiftXCoordAnn xoff ca =
  ca { caX = xoff + caX ca }


shiftX :: Int -> Expr CoordAnn -> Expr CoordAnn
shiftX xoff expr =
  case expr of
    Var ann name ->
      Var (shiftXCoordAnn xoff ann) name

    Abs ann name body ->
      Abs (shiftXCoordAnn xoff ann) name (shiftX (xoff + caXOff ann) body)

    App ann a b ->
      App (shiftXCoordAnn xoff ann) (shiftX (xoff + caXOff ann) a) (shiftX (xoff + caXOff ann) b)


fontWidth :: Int
fontWidth = 9


fontHeight :: Int
fontHeight = 14


yoffset :: Int
yoffset = 14


layoutExpr :: (Int, Int) -> Expr FBAnn -> Expr CoordAnn
layoutExpr (x,y) expr =
  case expr of
    Var ann name ->
      let
        width = T.length name * fontWidth
        coords = CoordAnn (x + (div width 2)) y width yoffset 0 ann
      in
        Var coords name

    Abs ann name body ->
      let
        width = (T.length name + 2) * fontWidth
        body1 = layoutExpr (x, y + (yoffset*2)) body
        ann' = getAnn body1
        width1 = max width (caW ann')
        xoff = if width < (caW ann') then 0 else (div (width - (caW ann')) 2)
        coords = CoordAnn (x + (div width1 2)) y width1 (caH ann' + (yoffset*2)) xoff ann
      in
        Abs coords name body1

    App ann a b ->
      let
        a1 = layoutExpr (x,y + (yoffset*2)) a
        annA = getAnn a1
        b1 = layoutExpr (x + (caW annA) + fontWidth, y + (yoffset*2)) b
        annB = getAnn b1
        width = caW annA + caW annB + fontWidth
        coords = CoordAnn (x + (div width 2)) y width (max (caH annA) (caH annB) + (yoffset*2)) 0 ann
      in
        App coords a1 b1


drawExpr :: ExprPath -> Expr CoordAnn -> ReactElementM 'EventHandlerCode ()
drawExpr path expr =
  case expr of
    Var ann name ->
      text_
        [ "x" &= show (caX ann)
        , "y" &= show (caY ann)
        , "textAnchor" &= ("middle" :: String)
        ] $ elemText name

    Abs ann name body -> do
      text_
        [ "x" &= show (caX ann)
        , "y" &= show (caY ann)
        , "textAnchor" &= ("middle" :: String)
        ] $ elemString ("\\" ++ (T.unpack name))
      drawLine (caX ann, caY ann + 1) (let ann' = getAnn body in (caX ann', caY ann' - fontHeight))
      drawExpr (True : path) body

    App ann a b ->
      let
        label =
          case a of
            Abs _ _ _ ->
              let
                --annB = getAnn b
                --collisions = Set.intersection (boundAnn $ caFBAnn ann') (freeAnn $ caFBAnn annB)
                isBeta = Set.null (conflictAnn $ caFBAnn ann)
              in
                text_
                  [ "x" &= show (caX ann)
                  , "y" &= show (caY ann)
                  , "textAnchor" &= ("middle" :: String)
                  , onClick $ \_ _ -> handleLambda (LambdaStepAt path)
                  , style [("fill", if isBeta then "blue" else "red")]
                  ] $ elemText "@"
            _
              ->
                text_
                  [ "x" &= show (caX ann)
                  , "y" &= show (caY ann)
                  , "textAnchor" &= ("middle" :: String)
                  ] $ elemText "@"
      in do
        label
        drawLine (caX ann, caY ann + 2) (let ann' = getAnn a in (caX ann', caY ann' - fontHeight))
        drawLine (caX ann, caY ann + 2) (let ann' = getAnn b in (caX ann', caY ann' - fontHeight))
        drawExpr (False : path) a
        drawExpr (True : path) b

