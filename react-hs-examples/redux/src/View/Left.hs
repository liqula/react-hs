{-# LANGUAGE DataKinds, OverloadedStrings #-} 

module View.Left (leftView) where

import Data.Foldable (for_)
import React.Flux
    ( View, elemString, form_, input_, li_, mkView, onChange, onSubmit
    , preventDefault, simpleHandler, target, ul_, ($=), (&=)
    )

import Base
import State.Left

leftView :: View '[Ref LeftState]
leftView = mkView "" $ \(LeftState c xs :-: _l) -> do
    form_ [ "key" $= "create"
          , onSubmit $ \_ -> preventDefault . (: []) . _l $ \(LeftState c' xs') ->
              pure . LeftState "" $ c' : xs'
          ] $
        input_ [ "key" $= "text"
               , "name" $= "value"
               , "type" $= "text"
               , "value" &= c
               , onChange $ \e -> simpleHandler . (: []) . _l $ \(LeftState _ xs') ->
                   pure $ LeftState (target e "value") xs'
               ]
    ul_ ["key" $= "list"] . for_ (zip [0 :: Int ..] xs) $ \(k, x) ->
        li_ ["key" &= k] $ elemString x
