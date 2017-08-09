module LambdaStore where

import React.Flux
import React.Flux.Ajax
import Control.Monad.State
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import System.Random
import qualified Data.JSString as JSS


import LambdaAST
import LambdaFBAnn
import LambdaParser
import LambdaEval


dispatchLambda :: LambdaAction -> [SomeStoreAction]
dispatchLambda a = [action @LambdaState a]


handleLambda :: LambdaAction -> ([SomeStoreAction], [EventModification])
handleLambda = simpleHandler . dispatchLambda


data LambdaState
  = LambdaState
  { lambdaExpr :: Maybe (Expr FBAnn)
  , lastInput :: Maybe (Expr FBAnn)
  , freshCounter :: Int
  , reductionCount :: Int
  , lambdaInputText :: String
  , runTicks :: Bool
  }
  deriving (Eq, Show, Typeable)


initialLambdaState :: LambdaState
initialLambdaState =
  LambdaState { lambdaExpr = Nothing
              , lastInput = Nothing
              , freshCounter = 0
              , reductionCount = 0
              , lambdaInputText = ""
              , runTicks = False
              }


data LambdaAction
  = LambdaStepAt ExprPath
  | LambdaStepsNormal Int
  | LambdaStepsRandom Int
  | LambdaReset
  | LambdaSetText String
  | LambdaTick
  | LambdaRunTicks Bool
  | LambdaLoad String
  | LambdaOnLoad String
  deriving (Show, Typeable, Generic)


instance StoreData LambdaState where
    type StoreAction LambdaState = LambdaAction
    transform = runLambdaAction


runLambdaAction :: LambdaAction -> LambdaState -> IO LambdaState
runLambdaAction a oldState =
  case a of
    LambdaTick ->
      if runTicks oldState then do
        runLambdaAction (LambdaStepsNormal 1) oldState
      else
        return oldState

    LambdaRunTicks b ->
      return oldState { runTicks = b }

    LambdaStepAt path ->
      case lambdaExpr oldState of
        Just expr ->
          let
            (expr', freshCounter') =
              runState (applyAtAndAnno (reverse path) reduceOrConvert expr) (freshCounter oldState)
          in
            return oldState { lambdaExpr = Just expr'
                            , freshCounter = freshCounter'
                            , reductionCount = reductionCount oldState + 1
                            }
        Nothing ->
          return oldState

    LambdaStepsRandom n ->
      case lambdaExpr oldState of
        Just expr -> do
          gen <- getStdGen
          let
            (expr', freshCounter', gen') =
              runNextRandom gen n expr (freshCounter oldState)
          setStdGen gen'
          return oldState { lambdaExpr = Just expr'
                          , freshCounter = freshCounter'
                          , reductionCount = reductionCount oldState + n
                          }
        Nothing ->
          return oldState

    LambdaStepsNormal n ->
      case lambdaExpr oldState of
        Just expr ->
          let
            (expr', freshCounter', moreToDo) =
              runNextNormal n expr (freshCounter oldState)
          in
            return oldState { lambdaExpr = Just expr'
                            , freshCounter = freshCounter'
                            , reductionCount = reductionCount oldState + n
                            , runTicks = (runTicks oldState && moreToDo)
                            }
        Nothing ->
          return oldState

    LambdaReset ->
      return oldState { lambdaExpr = lastInput oldState
                      , reductionCount = 0
                      , runTicks = False
                      }

    LambdaSetText s ->
      case parseExpr s of
        Nothing ->
          return $ oldState { lambdaInputText = s
                            , runTicks = False
                            }
        Just e ->
          let
            e' = annoFBAnn e
            r  = 1 + maxNameNumber e
          in
            return oldState { lambdaExpr = Just e'
                            , lastInput = Just e'
                            , freshCounter = r
                            , reductionCount = 0
                            , lambdaInputText = s
                            , runTicks = False
                            }

    LambdaOnLoad s ->
      case parseExpr s of
        Nothing ->
          return oldState { lambdaInputText = s
                          , runTicks = False
                          }
        Just e ->
          let
            e' = annoFBAnn e
            r  = 1 + maxNameNumber e
          in
            return oldState { lambdaExpr = Just e'
                            , lastInput = Just e'
                            , freshCounter = r
                            , reductionCount = 0
                            , lambdaInputText = s
                            , runTicks = False
                            }

    LambdaLoad url -> do
      let req = AjaxRequest "GET" (JSS.pack url) NoTimeout [("Accept", "application/json")] ""
      ajax req $ \rsp -> return $ dispatchLambda $ LambdaOnLoad $ JSS.unpack $ respResponseText rsp
      return oldState

