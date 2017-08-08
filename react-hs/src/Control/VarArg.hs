{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.VarArg
  ( VarArg
  , mapVarArg
  ) where

import qualified Control.Lens as Lens
import Control.Lens


type family VarArg (props :: [*]) e where
  VarArg '[] e = e
  VarArg (a ': as) e = a -> VarArg as e

-- | see https://stackoverflow.com/questions/45178068/fmap-over-variable-argument-function/
mapVarArg :: forall props e e' . VarArgIso props => (e -> e') -> VarArg props e -> VarArg props e'
mapVarArg f = Lens.under (Lens.from (varArgIso @props)) (fmap f)

data VarArgD (props :: [*]) e where
  DNil  :: e -> VarArgD '[] e
  DCons :: (a -> VarArgD as e) -> VarArgD (a ': as) e

class VarArgIso (props :: [*]) where
  varArgIso :: Lens.Iso (VarArg props e) (VarArg props e') (VarArgD props e) (VarArgD props e')

instance VarArgIso '[] where
  varArgIso = iso DNil (\(DNil x) -> x)

instance VarArgIso as => VarArgIso (a ': as) where
  varArgIso = iso (\f -> DCons ((^. varArgIso) . f)) (\(DCons f) -> ((^. Lens.from varArgIso) . f))

instance Functor (VarArgD props) where
  fmap f (DNil a)  = DNil (f a)
  fmap f (DCons g) = DCons (fmap f . g)
