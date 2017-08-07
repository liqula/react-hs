{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

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
