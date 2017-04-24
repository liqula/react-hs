{-# LANGUAGE TypeApplications #-}
module TodoDispatcher (dispatchTodo) where

import React.Flux
import TodoStore

dispatchTodo :: TodoAction -> [SomeStoreAction]
dispatchTodo a = [someStoreAction @TodoState a]
