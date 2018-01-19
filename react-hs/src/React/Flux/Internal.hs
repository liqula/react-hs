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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Internal module for React.Flux
--
-- Normally you should not need to use anything in this module.  This module is only needed if you have
-- complicated interaction with third-party javascript rendering code.
module React.Flux.Internal
  ( module React.Internal
  , StoreArg
  , StoreField
  , allEq
  , AllEq(..)
  ) where

import Data.Typeable

import GHCJS.Foreign.Callback
import GHCJS.Types (JSVal, JSString)
import GHCJS.Marshal (ToJSVal(..))

import React.Internal

-- | If you want to pass a store value into a component via 'mkControllerView', make entry in the
-- type list you apply have type @StoreArg <store>@.  See also: 'StoreField'; see test client for an
-- example how to use this.
data StoreArg store

-- | If you want to pass a *part of a* store value into a component via 'mkControllerView', make entry in the
-- type list you apply have type @StoreField <store> "<fieldname>" <fieldtype>@ (field name is a
-- type-level string literal).  See also: 'StoreArg'; see test client for an example how to use
-- this.
data StoreField store (fieldname :: k) fieldtype

allEq :: AllEq t => Proxy t -> IO (Callback ForeignEq)
allEq proxy = syncCallback2' (\jsa jsb -> toJSVal =<< allEq_ proxy 0 jsa jsb)

type family StoreType a where
  StoreType (StoreArg st) = st
  StoreType (StoreField st fn ft) = ft
  StoreType a = a

class AllEq t where
  allEq_ :: Proxy t -> Int -> ForeignEq_

instance AllEq '[] where
  allEq_ Proxy _ _ _ = pure True

instance (Typeable (StoreType t), Eq (StoreType t), AllEq ts)
      => AllEq (t ': ts) where
  allEq_ Proxy i jsas jsbs = do
    let jsa = js_findFromArray i jsas
        jsb = js_findFromArray i jsbs
    (&&) <$> singleEq_ (Proxy :: Proxy (StoreType t)) jsa jsb
         <*> allEq_    (Proxy :: Proxy ts) (i + 1) jsas jsbs

#ifdef __GHCJS__

-- | (This is similar to findFromState, but less specific, and more "pure".  not sure if we can merge
-- the two?)
foreign import javascript unsafe
  "$2[$1]"
  js_findFromArray :: Int -> JSVal -> JSVal

#else

js_findFromArray :: Int -> JSVal -> JSVal
js_findFromArray _ _ = error "js_findFromArray only works with GHCJS"

#endif
