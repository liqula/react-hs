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

{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Internal module containing the store definitions.
module React.Flux.Store (
    ReactStoreRef(..)
  , StoreData(..)

  -- * Old Stores
  , SomeStoreAction(..)
  , executeAction

  -- * New Stores
  , NewReactStoreHS(..)
  , NewReactStore(..)
  , action
  , registerInitialStore
  , transformStore
  , readStoreData

  -- * Util
  , typeJsKey
) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Data.Typeable
import GHC.Generics (Generic)
import React.Flux.Internal (unsafeDerefExport)
import Data.Monoid ((<>))
import GHCJS.Foreign.Export
import GHCJS.Types (JSVal, IsJSVal, JSString)
import GHC.Fingerprint.Type
import qualified Data.JSString as JSString
import Data.Word (Word64)

#ifdef __GHCJS__
import qualified Data.JSString.Int as JSString (decimal)
#endif

-- | A store contains application state, receives actions from the dispatcher, and notifies
-- controller-views to re-render themselves.  You can have multiple stores; it should be the case
-- that all of the state required to render the page is contained in the stores.  A store keeps a
-- global reference to a value of type @storeData@, which must be an instance of 'StoreData'.
--
-- Stores also work when compiled with GHC instead of GHCJS.  When compiled with GHC, the store is
-- just an MVar containing the store data and there are no controller views.  'alterStore' can still
-- be used, but it just 'transform's the store and does not notify any controller-views since there
-- are none.  Compiling with GHC instead of GHCJS can be helpful for unit testing, although GHCJS
-- plus node can also be used for unit testing.
--
-- >data Todo = Todo {
-- >    todoText :: Text
-- >  , todoComplete :: Bool
-- >  , todoIsEditing :: Bool
-- >} deriving (Show, Typeable)
-- >
-- >newtype TodoState = TodoState {
-- >    todoList :: [(Int, Todo)]
-- >} deriving (Show, Typeable)
-- >
-- >data TodoAction = TodoCreate Text
-- >                | TodoDelete Int
-- >                | TodoEdit Int
-- >                | UpdateText Int Text
-- >                | ToggleAllComplete
-- >                | TodoSetComplete Int Bool
-- >                | ClearCompletedTodos
-- >  deriving (Show, Typeable, Generic)
-- >
-- >instance StoreData TodoState where
-- >    type StoreAction TodoState = TodoAction
-- >    transform action (TodoState todos) = ...
-- >
-- >initTodoStore :: IO ()
-- >initTodoStore = registerInitialStore $ TodoState
-- >    [ (0, Todo "Learn react" True False)
-- >    , (1, Todo "Learn react-hs" False False)
-- >    ]
class Typeable storeData => StoreData storeData where
    -- | The actions that this store accepts
    type StoreAction storeData

    -- | Transform the store data according to the action.  This is the only place in your app where
    -- @IO@ should occur.  The transform function should complete quickly, since the UI will not be
    -- re-rendered until the transform is complete.  Therefore, if you need to perform some longer
    -- action, you should fork a thread from inside 'transform'.  The thread can then call 'alterStore'
    -- with another action with the result of its computation.  This is very common to communicate with
    -- the backend using AJAX.  Indeed, the 'React.Flux.Combinators.jsonAjax' utility function
    -- implements exactly this strategy since it is so common.
    --
    -- Note that if the transform throws an exception, the transform will be aborted and the old
    -- store data will be kept unchanged.  The exception will then be thrown from 'alterStore'.
    --
    -- For the best performance, care should be taken in only modifying the part of the store data
    -- that changed (see below for more information on performance).
    transform :: StoreAction storeData -> storeData -> IO storeData

-- | An existential type for some store action.  It is used as the output of the dispatcher.
data SomeStoreAction = forall storeData. (StoreData storeData)
     => SomeNewStoreAction (Proxy storeData) (StoreAction storeData)

-- | Create some store action.  You must use a type-argument to specify the storeData type (because technically, the same
-- store action type could be used for different stores).  I strongly suggest you keep a one-to-one correspondence between
-- stores and store actions, but GHC does not know that.  For example,
--
-- >todoAction :: TodoAction -> SomeStoreAction
-- >todoAction a = action @TodoStore a
action :: forall storeData. (StoreData storeData) => StoreAction storeData -> SomeStoreAction
action = SomeNewStoreAction (Proxy :: Proxy storeData)

-- | Call 'alterStore' on the store and action.
executeAction :: SomeStoreAction -> IO ()
executeAction (SomeNewStoreAction p a) = transformStore p a

--------------------------------------------------------------------------------
-- Old Version Store Definition
--------------------------------------------------------------------------------

-- | This type is used to represent the foreign javascript object part of the store.
newtype ReactStoreRef storeData = ReactStoreRef JSVal
  deriving (Generic)
instance IsJSVal (ReactStoreRef storeData)

data NewReactStoreHS = NewReactStoreHS {
    newStoreLock :: MVar ()
  } deriving Typeable

----------------------------------------------------------------------------------------------------
-- Store operations
----------------------------------------------------------------------------------------------------

-- | The new type of stores, introduced in version 1.3, keep the data in a javascript dictionary indexed
-- by the fingerprint of the type.  This allows any code to lookup the store by knowing the type.
newtype NewReactStore storeData = NewReactStore JSVal
instance IsJSVal (NewReactStore storeData)

-- | New stores are kept in a javascript dictionary by type.  This computes the key into this dictionary.
--
-- FIXME: make the return value of this a newtype StoreKey, make typeJsKey private, and make
-- Views.stateForView use StoreKey as hashmap keys.
storeJsKey :: Typeable ty => Proxy ty -> JSString
storeJsKey p = typeJsKey (typeRep p)

typeJsKey :: TypeRep -> JSString
typeJsKey t = decimal_workaround_570 f1 <> "-" <> decimal_workaround_570 f2
  where
    Fingerprint f1 f2 = typeRepFingerprint t

-- | Register the initial store data.  This function must be called exactly once from your main function before
-- the initial rendering occurs.  Store data is global and so there can be only one store data value for each
-- store type.
--
-- FIXME: why not @storeE <- export =<< newMVar initial@?  i think the reason to keep the lock
-- separately in 'NewReactStoreHS' is just due to the fact that too much code is on the javascript
-- side.  The MVar should contain the entirety of @{ sdata: ..., views: ..., hs: <lock> }@, which
-- would eliminate the need fo the lock.  (medium-sized refactoring if we're lucky.)
registerInitialStore :: forall storeData. (Typeable storeData, StoreData storeData) => storeData -> IO ()
registerInitialStore initial = do
  sdataE <- export initial
  storeE <- export . NewReactStoreHS =<< newMVar ()
  js_createNewStore (storeJsKey (Proxy :: Proxy storeData)) sdataE storeE

-- | First, 'transform' the store data according to the given action and then notify all registered
-- controller-views to re-render themselves.
--
-- Only a single thread can be transforming the store at any one time, so this function will block
-- on an 'MVar' waiting for a previous transform to complete if one is in process.
--
-- This function will 'error' if 'registerInitialStore' has not been called.
transformStore :: forall storeData. StoreData storeData => Proxy storeData -> StoreAction storeData -> IO ()
transformStore _ a = do
  store :: NewReactStore storeData <- js_getNewStore (storeJsKey (Proxy :: Proxy storeData))
  storeHS <- getNewStoreHS store
  modifyMVar_ (newStoreLock storeHS) $ \() -> do
    oldData :: storeData <- js_getNewStoreData store >>= unsafeDerefExport "transformStore"
    newData :: storeData <- transform a oldData
    js_updateNewStore store =<< export newData

-- | Obtain the store data from a store.  Note that the store data is stored in an MVar, so
-- 'readStoreData' can block since it uses 'readMVar'.  The 'MVar' is empty exactly when the store is
-- being transformed, so there is a possiblity of deadlock if two stores try and access each other's
-- data during transformation.
--
-- This function will 'error' if 'registerInitialStore' has not been called.
readStoreData :: forall storeData. (Typeable storeData, StoreData storeData) => IO storeData
readStoreData = do
  store :: NewReactStore storeData <- js_getNewStore (storeJsKey (Proxy :: Proxy storeData))
  js_getNewStoreData store >>= unsafeDerefExport "readStoreData"

getNewStoreHS :: NewReactStore storeData -> IO NewReactStoreHS
getNewStoreHS s = js_getNewStoreHS s >>= unsafeDerefExport "getNewStoreHS"
{-# NOINLINE getNewStoreHS #-}

-- | See https://github.com/ghcjs/ghcjs/issues/570 for details.
-- FIXME: this is probably needed any more in lts-8.11 and the ghcjs version we use.
decimal_workaround_570 :: Word64 -> JSString
decimal_workaround_570 w = dropleadingzeros . mconcat $ showpadded <$> chunks
  where
    n :: Integer -> Integer
    n i = 10^(5 * i)

    chunks :: [Integer]
    chunks =
      [ (fromIntegral w `div` (n 3)) `mod` n 1
      , (fromIntegral w `div` (n 2)) `mod` n 1
      , (fromIntegral w `div` (n 1)) `mod` n 1
      , fromIntegral w               `mod` n 1
      ]

    showpadded :: Integer -> JSString
    showpadded i = JSString.reverse . JSString.take 5 . JSString.reverse
                 $ JSString.pack "00000" <> decimal i


    dropleadingzeros :: JSString -> JSString
    dropleadingzeros = JSString.dropWhile (== '0')

#ifdef __GHCJS__

decimal :: Integer -> JSString
decimal = JSString.decimal

foreign import javascript unsafe
  "hsreact$storedata[$1]"
  js_getNewStore :: JSString -> IO (NewReactStore storeData)

foreign import javascript unsafe
  "$1.hs"
  js_getNewStoreHS :: NewReactStore storeData -> IO (Export NewReactStoreHS)

foreign import javascript unsafe
  "$1.sdata"
  js_getNewStoreData :: NewReactStore storeData -> IO (Export storeData)

foreign import javascript unsafe
  "hsreact$storedata[$1] = {sdata: $2, views: {}, hs: $3}"
  js_createNewStore :: JSString -> Export storeData -> Export NewReactStoreHS -> IO ()

-- | Perform the update, swapping the old export and the new export and then notifying the component views.
foreign import javascript unsafe
  "hsreact$transform_new_store($1, $2)"
  js_updateNewStore :: NewReactStore storeData -> Export storeData -> IO ()

#else

decimal :: Integer -> JSString
decimal = error "decimal only works with GHCJS"

js_getNewStore :: JSString -> IO (NewReactStore storeData)
js_getNewStore _ = error "js_getNewStore only works with GHCJS"

js_getNewStoreHS :: NewReactStore storeData -> IO (Export NewReactStoreHS)
js_getNewStoreHS _ = error "js_getNewStoreHS only works with GHCJS"

js_getNewStoreData :: NewReactStore storeData -> IO (Export storeData)
js_getNewStoreData _ = error "js_getNewStoreData only works with GHCJS"

js_createNewStore :: JSString -> Export storeData -> Export NewReactStoreHS -> IO ()
js_createNewStore _ _ _ = error "js_createNewStore only works with GHCJS"

js_updateNewStore :: NewReactStore storeData -> Export storeData -> IO ()
js_updateNewStore _ _ = error "js_updateNewStore only works with GHCJS"

#endif
