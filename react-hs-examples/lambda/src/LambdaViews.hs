{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | The views for the TODO app
module LambdaViews where

import React.Flux
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.List as List

import LambdaAST
import LambdaFBAnn
--import LambdaDispatcher
import LambdaStore
import LambdaSVG


-- | The controller view and also the top level of the LAMBDA app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
lambdaApp :: View '[]
lambdaApp = mkControllerView @'[StoreArg LambdaState] "lambda app" $ \lambdaState ->
    div_ $ do
        lambdaView (lambdaState)


lambdaView :: LambdaState -> ReactElementM 'EventHandlerCode ()
lambdaView ls = section_ ["id" $= "lambda"] $ do
    h2_ [ "key" &= ("h1" :: String)] "lambda"
    div_ [ "key" &= ("d1" :: String)] $ textarea_
      [ onInput $ \evt -> handleLambda (LambdaSetText $ target evt "value")
      , onChange $ \evt -> handleLambda (LambdaSetText $ target evt "value")
      , "value" &= lambdaInputText ls
      , "className" &= ("lambdatextinput" :: String)
      ] mempty
    div_ [ "key" &= ("d2" :: String)] $ do
      button_ [ "key" &= ("b1" :: String), onClick $ \_ _ -> handleLambda (LambdaLoad "example1.txt") ] $ elemString "load 1"
      button_ [ "key" &= ("b2" :: String), onClick $ \_ _ -> handleLambda (LambdaLoad "example2.txt") ] $ elemString "load 2"
    div_ [ "key" &= ("d3" :: String)] $ do
      button_ [ "key" &= ("b3" :: String), onClick $ \_ _ -> handleLambda (LambdaRunTicks $ not (runTicks ls)), "disabled" @= (maybe True (const False) (lambdaExpr ls)) ] $ elemString (if runTicks ls then "stop" else "run")
      button_ [ "key" &= ("b4" :: String), onClick $ \_ _ -> handleLambda (LambdaReset), "disabled" @= (maybe True (const False) (lastInput ls)) ] $ elemString "reset"
    div_ [ "key" &= ("d4" :: String)] $ do
      button_ [ "key" &= ("b5" :: String), onClick $ \_ _ -> handleLambda (LambdaStepsNormal 1) ] $ elemString "normal reduction"
      button_ [ "key" &= ("b6" :: String), onClick $ \_ _ -> handleLambda (LambdaStepsNormal 100) ] $ elemString "100 normal reductions"
      button_ [ "key" &= ("b7" :: String), onClick $ \_ _ -> handleLambda (LambdaStepsNormal 500) ] $ elemString "500 normal reductions"
      button_ [ "key" &= ("b8" :: String), onClick $ \_ _ -> handleLambda (LambdaStepsRandom 1) ] $ elemString "random reduction"
      button_ [ "key" &= ("b9" :: String), onClick $ \_ _ -> handleLambda (LambdaStepsRandom 50) ] $ elemString "50 random reductions"
    div_ [ "key" &= ("d5" :: String)] $ elemString (show $ reductionCount ls)
    div_ [ "key" &= ("d6" :: String)] $ viewExpr (lambdaExpr ls)


viewExpr :: Maybe (Expr FBAnn) -> ReactElementM 'EventHandlerCode ()
viewExpr Nothing = elemString ""
viewExpr (Just expr) = do
  div_ [ "key" &= ("dd1" :: String)] $ renderExpr [] expr
  div_ [ "key" &= ("dd2" :: String)] $ renderExprSVG expr 0.5


renderExpr :: [Bool] -> Expr FBAnn -> ReactElementM 'EventHandlerCode ()
renderExpr path expr =
  case expr of
    Var _ name ->
      elemText name
    Abs ann name body -> do
      span_
        [ "title" &= (List.intercalate " " $ map (T.unpack) $ Set.toList $ freeAnn ann)
        ] $ elemString ("\\" ++ T.unpack name ++ ". ")
      renderExpr (True : path) body
    App ann fun@Abs{} arg -> do
      span_
        [ onClick $ \_ _ -> handleLambda (LambdaStepAt path)
        , style [("color",if Set.null (conflictAnn ann) then "blue" else "red")]
        , "title" &= (List.intercalate " " $ map (T.unpack) $ Set.toList $ freeAnn ann)
        ] "@"
      renderExprApp path fun arg
    App ann fun arg -> do
      span_
        [ "title" &= (List.intercalate " " $ map (T.unpack) $ Set.toList $ freeAnn ann)
        ] "@"
      renderExprApp path fun arg


renderExprApp :: [Bool] -> Expr FBAnn -> Expr FBAnn -> ReactElementM 'EventHandlerCode ()
renderExprApp path fun arg = do
  renderExpr (False : path) fun
  elemString " "
  renderExpr (True : path) arg

