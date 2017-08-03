module Main where

import React.Flux
import TodoViews
import TodoStore

main :: IO ()
main = do
  registerInitialStore $ initialTodos
  reactRenderView "todoapp" todoApp
