{-# LANGUAGE DataKinds, OverloadedStrings #-}

module View.Right (rightView) where

import React.Flux (View, button_, div_, elemString, mkView, onClick, simpleHandler, ($=))

import Base

rightView :: View '[Ref Int]
rightView = mkView "" $ \(x :-: _x) -> do
    div_ ["key" $= "display"] . elemString $ show x
    div_ ["key" $= "increment"] $
        button_ [ onClick $ \_ _ -> simpleHandler . (: []) . _x $ \x' ->
                    pure $ x' + 1
                ] "Increment"
    div_ ["key" $= "decrement"] $
        button_ [ onClick $ \_ _ -> simpleHandler . (: []) . _x $ \x' ->
                    pure $ x' - 1
                ] "Decrement"
