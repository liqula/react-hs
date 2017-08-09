module Main where

import React.Flux
import React.Flux.Ajax
import LambdaViews
import LambdaStore
import Control.Concurrent


lambdaAction :: LambdaAction -> SomeStoreAction
lambdaAction a = action @LambdaState a

main :: IO ()
main = do
  initAjax
  registerInitialStore $ initialLambdaState
  reactRenderView "lambdaapp" lambdaApp
  let
    ticker = do
      executeAction (lambdaAction LambdaTick)
      threadDelay 40000
      ticker
  _ <- forkIO ticker
  return ()
