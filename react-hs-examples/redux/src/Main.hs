{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, TypeFamilies #-}

module Main (main) where

import React.Flux
    ( StoreAction, StoreArg, StoreData, View, action, mkControllerView
    , reactRenderView, registerInitialStore, transform, view_
    )

import Base
import Init
import State
import View

main :: IO ()
main = do
    registerInitialStore . Store =<< getInitial
    reactRenderView "content" controller

controller :: View '[]
controller = mkControllerView @'[StoreArg Store] "" $ \(Store s) ->
    view_ view "" $ s :-: action @Store . _state
    where _state f (Store s) = Store <$> f s

newtype Store = Store State
    deriving Eq

instance StoreData Store where
    type StoreAction Store = Store -> IO Store
    transform = id
