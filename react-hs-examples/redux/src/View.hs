{-# LANGUAGE DataKinds, OverloadedStrings #-}

module View (view) where

import React.Flux (View, mkView, view_)

import Base
import State
import View.Left
import View.Right

view :: View '[Ref State]
view = mkView "" $ \(State xs n :-: _s) -> do
    view_ leftView "left" $ xs :-: _s . _left
    view_ rightView "right" $ n :-: _s . _right
  where
    _left f (State xs n) = (`State` n) <$> f xs
    _right f (State xs n) = State xs <$> f n
