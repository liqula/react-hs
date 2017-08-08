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

-- | Internal module containing the view definitions
{-# OPTIONS_GHC -fno-warn-orphans #-}
module React.Flux.Views
  ( View(..)
  , ViewPropsToElement
  , ViewEventHandler
  , StatefulViewEventHandler
  , liftViewToStateHandler
  , mkView
  , view_
  , mkStatefulView
  , StoreArg
  , StoreField
  , HasField(..)
  , ControllerViewToElement
  , mkControllerView
  , exportReactViewToJavaScript
  , callbackRenderingView
  , ViewProps(..)
  , ControllerViewStores(..)
  , ExportViewProps(..)
  , StoreToState(..)
  , JsState(..)
  , Artifact(..), Artifacts(..)
  ) where

import Data.Typeable
import qualified Data.HashMap.Strict as M

import React.Flux.Store
import React.Flux.Internal

import Control.Monad
import Control.VarArg
import System.IO.Unsafe (unsafePerformIO)
import JavaScript.Array
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Export
import GHCJS.Types (JSVal, IsJSVal, nullRef)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import qualified JavaScript.Array as JSA


---------------------------------------------------------------------------------
-- Public API
---------------------------------------------------------------------------------

-- | A view is conceptually a rendering function from @props@ and some internal state to a tree of
-- elements.  The function receives a value of type @props@ from its parent in the virtual DOM.
-- Additionally, the rendering function can depend on some internal state or store data.  Based on
-- the @props@ and the internal state, the rendering function produces a virtual tree of elements
-- which React then reconciles with the browser DOM.
--
-- This module supports 3 kinds of views.  All of the views provided by this module are pure, in the
-- sense that the rendering function and event handlers cannot perform any IO.  All IO occurs inside
-- the 'transform' function of a store.
--
-- Due to React limitations (see <https://github.com/facebook/react/issues/2127 issue2127>), React
-- views must have a single top-level element.  If your haskell code returns multiple top-level
-- elements, react-flux will wrap them in a container @div@.  You should not rely on this and
-- instead make sure each view returns only a single top-level element (such as @todoItem@ in
-- react-hs-examples returning only a single @li@ element).
newtype View (props :: [*]) = View (ReactViewRef ())

type ViewPropsToElement  (props :: [*]) (handler :: EventHandlerCode *) = VarArg props (ReactElementM handler ())

-- | Event handlers in a controller-view and a view transform events into actions, but are not
-- allowed to perform any 'IO'.
type ViewEventHandler = [SomeStoreAction]

-- | A stateful-view event handler produces a list of store actions and potentially a new state.  If
-- the new state is nothing, no change is made to the state (which allows an optimization in that we
-- do not need to re-render the view).
--
-- Changing the state causes a re-render which will cause a new event handler to be created.  If the
-- handler closes over the state passed into the rendering function, there is a race if multiple
-- events occur before React causes a re-render.  Therefore, the handler takes the current state as
-- input.  Your handlers therefore should ignore the state passed into the render function and
-- instead use the state passed directly to the handler.
type StatefulViewEventHandler state = state -> ([SomeStoreAction], Maybe state)

-- FIXME: make EventHandlerType closed
type instance EventHandlerType 'EventHandlerCode = ViewEventHandler
type instance EventHandlerType ('StatefulEventHandlerCode a) = StatefulViewEventHandler a

-- | Change the event handler from 'ViewEventHandler' to 'StatefulViewEventHandler' to allow you to embed
-- combinators with 'ViewEventHandler's into a stateful view.  Each such lifted handler makes no change to
-- the state.
liftViewToStateHandler :: ReactElementM 'EventHandlerCode a -> ReactElementM ('StatefulEventHandlerCode st) a
liftViewToStateHandler = transHandler (\h _ -> (h, Nothing))

class HasField (x :: k) r a | x r -> a where
  getField :: r -> a

type family StrictStoreType a where
  StrictStoreType (StoreArg store) = store
  StrictStoreType (StoreField _store _field a) = a

-- sadly it cannot be defined as Map StrictStoreType
type family StrictStoreTypes (stores :: [*]) where
  StrictStoreTypes '[] = '[]
  StrictStoreTypes (t ': ts) = StrictStoreType t ': StrictStoreTypes ts

type ControllerViewToElement (stores :: [*]) (props :: [*]) (handler :: EventHandlerCode *)
  = VarArg (StrictStoreTypes stores) (ViewPropsToElement props handler)

--------------------------------------------------------------------------------
-- View Props Classes
--------------------------------------------------------------------------------

class ViewProps (props :: [*]) (handler :: EventHandlerCode *) where
  viewPropsToJs :: ReactViewRef () -> JSString -> (NewJsProps -> IO ()) -> ViewPropsToElement props handler
  applyViewPropsFromJs :: ViewPropsToElement props handler -> NewJsProps -> Int -> IO (ReactElementM handler ())

class ExportViewProps (props :: [*]) handler where
  applyViewPropsFromArray :: JSArray -> Int -> NewJsProps -> IO ()

instance ViewProps '[] handler where
  viewPropsToJs ref k props = elementToM () $ NewViewElement ref k props
  applyViewPropsFromJs x _ _ = return x
  {-# INLINE viewPropsToJs #-}
  {-# INLINE applyViewPropsFromJs #-}

instance ExportViewProps '[] handler where
  applyViewPropsFromArray _ _ _ = return ()

instance (ViewProps rest handler, Typeable a)
      => ViewProps (a ': (rest :: [*])) handler where
  viewPropsToJs ref k props = \a -> viewPropsToJs @rest @handler ref k (\p -> props p >> pushProp a p)
  {-# INLINE viewPropsToJs #-}

  applyViewPropsFromJs f props i = do
    val <- getProp props i
    applyViewPropsFromJs @rest (f val) props (i+1)
  {-# INLINE applyViewPropsFromJs #-}

instance forall handler (rest :: [*]) a. (ExportViewProps rest handler, Typeable a, FromJSVal a)
      => ExportViewProps (a ': rest) handler where
  applyViewPropsFromArray inputArr k outputArr =
    do ma <- fromJSVal $ if k >= JSA.length inputArr then nullRef else JSA.index k inputArr
       a :: a <- maybe (error "Unable to decode callback argument") return ma
       pushProp a outputArr
       applyViewPropsFromArray @rest @handler inputArr (k+1) outputArr

--------------------------------------------------------------------------------
-- Controller View Props Classes
--------------------------------------------------------------------------------

newtype JsState = JsState JSVal -- javascript map from store typerep fingerprint to value

data StoreToState = StoreState Int
                  | StoreDerivedState
                    { storeDerivedOffset :: Int
                    , storeToStateCallback :: IO (Callback (JSVal -> IO ()))
                    }

class ControllerViewStores (stores :: [*]) props (handler :: EventHandlerCode *) where
  applyControllerViewFromJs :: ViewProps props handler
                            => ControllerViewToElement stores props handler
                            -> JsState
                            -> NewJsProps
                            -> Int
                            -> IO (ReactElementM handler ())
  stateForView :: Int -> M.HashMap TypeRep [StoreToState]


instance ControllerViewStores '[] props handler where
  applyControllerViewFromJs f _ props _ = applyViewPropsFromJs @props @handler f props 0
  stateForView _ = mempty
  {-# INLINE applyControllerViewFromJs #-}
  {-# INLINE stateForView #-}

instance (ControllerViewStores rest props handler, StoreData store, Typeable store)
   => ControllerViewStores (StoreArg store ': (rest :: [*])) props handler where
  applyControllerViewFromJs f st props i = do
    sval <- findFromState i st
    applyControllerViewFromJs @rest @props @handler (f sval) st props (i+1)

  stateForView i = M.insertWith (++) storeT [StoreState i] (stateForView @rest @props @handler (i+1))
    where
      storeT = typeRep (Proxy :: Proxy store)
  {-# INLINE applyControllerViewFromJs #-}
  {-# INLINE stateForView #-}

instance ( ControllerViewStores rest props handler
         , StoreData store, HasField field store a, Typeable store, Typeable a
         )
   => ControllerViewStores (StoreField store field a ': (rest :: [*])) props handler where
  applyControllerViewFromJs f st props i = do
    sval <- findFromState i st
    applyControllerViewFromJs @rest @props @handler (f sval) st props (i+1)

  stateForView i = M.insertWith (++) storeT [StoreDerivedState i derive] (stateForView @rest @props @handler (i+1))
    where
      storeT = typeRep (Proxy :: Proxy store)
      derive =
        syncCallback1 ThrowWouldBlock $ \arg -> do
          storeD :: store <- getStoreJs arg
          let a :: a = getField @field storeD
          aE <- fakeExport $! a
          js_setDeriveOutput arg aE
  {-# INLINE applyControllerViewFromJs #-}
  {-# INLINE stateForView #-}

getStoreJs :: Typeable store => JSVal -> IO store
getStoreJs arg = js_getDeriveInput arg >>= unsafeDerefExport "getStoreJs"
{-# NOINLINE getStoreJs #-} -- if this is inlined, GHCJS does not properly compile the getField callback


---------------------------------------------------------------------------------
-- Public API Implementation
---------------------------------------------------------------------------------

-- | Create a standard Haskell combinator for a 'View'.
view_ :: forall props handler. ViewProps (props :: [*]) handler
    => View props -> JSString -> ViewPropsToElement props handler
view_ (View ref) key = viewPropsToJs @props @handler ref key (const $ return ())

-- | Make a simple 'View'.  See also: 'mkStatefulView', 'mkControllerView'.
mkView :: forall (props :: [*]). (ViewProps props 'EventHandlerCode, Typeable props, AllEq props)
    => JSString -> ViewPropsToElement props 'EventHandlerCode -> View props
mkView name buildNode = unsafePerformIO $ do
  renderCb <- syncCallback2 ContinueAsync $ \thisRef argRef -> do
    let this = ReactThis thisRef
        arg = RenderCbArg argRef
    props <- js_PropsList this
    node <- applyViewPropsFromJs @props @'EventHandlerCode buildNode props 0
    (element, evtCallbacks) <- mkReactElement (runViewHandler this) this node
    evtCallbacksRef <- toJSVal evtCallbacks
    js_RenderCbSetResults arg evtCallbacksRef element

  propsEq <- allEq (Proxy :: Proxy props)
  View <$> js_createNewView name renderCb propsEq
{-# NOINLINE mkView #-}

mkStatefulView :: forall (state :: *) (props :: [*]).
                  (Typeable state, Typeable state, Eq state,
                   ViewProps props ('StatefulEventHandlerCode state), Typeable props, AllEq props)
               => JSString -- ^ A name for this view, used only for debugging/console logging
               -> state -- ^ The initial state
               -> (state -> ViewPropsToElement props ('StatefulEventHandlerCode state))
               -> View props
mkStatefulView name initial buildNode = unsafePerformIO $ do
  initialRef <- export initial
  renderCb <- syncCallback2 ContinueAsync $ \thisRef argRef -> do
    let this = ReactThis thisRef
        arg = RenderCbArg argRef
    props <- js_PropsList this
    state <- js_ReactGetState this >>= unsafeDerefExport "mkStatefulView"
    node <- applyViewPropsFromJs @props (buildNode state) props 0
    (element, evtCallbacks) <- mkReactElement (runStateViewHandler this) this node
    evtCallbacksRef <- toJSVal evtCallbacks
    js_RenderCbSetResults arg evtCallbacksRef element

  propsEq <- allEq (Proxy :: Proxy props)
  stateEq <- singleEq (Proxy :: Proxy state)
  View <$> js_createNewStatefulView name initialRef renderCb propsEq stateEq
{-# NOINLINE mkStatefulView #-}

mkControllerView :: forall (stores :: [*]) (props :: [*]).
                    (ControllerViewStores stores props 'EventHandlerCode, Typeable stores, AllEq stores,
                     ViewProps props 'EventHandlerCode, Typeable props, AllEq props)
                 => JSString -> ControllerViewToElement stores props 'EventHandlerCode -> View props
mkControllerView name buildNode = unsafePerformIO $ do
  renderCb <- syncCallback2 ContinueAsync $ \thisRef argRef -> do
    let this = ReactThis thisRef
        arg = RenderCbArg argRef
    props <- js_PropsList this
    st <- js_NewStateDict this
    node <- applyControllerViewFromJs @stores @props buildNode st props 0
    (element, evtCallbacks) <- mkReactElement (runViewHandler this) this node
    evtCallbacksRef <- toJSVal evtCallbacks
    js_RenderCbSetResults arg evtCallbacksRef element

  -- what's with 'artifacts'?  -- the hashmap constructed by 'stateForView' maps single store types
  -- to its positions in the list type of store types.  'getInitialState' (in view.js) goes through
  -- the types, looks up the resp. store and the resp. location in the argument list of the
  -- component, and stores it in the aragument object (indexed by locations).  you can also pass one
  -- global state in several places.  "straight-forward", hah?!  (:
  --
  -- FIXME: artifacts are used in mk_new_ctrl_view, so it makes sense for them to be a foreign
  -- value, but does it have to be *constructed* foreign?  we could make it a haskell value with a
  -- ToJSVal instance.

  artifacts <- js_emptyArtifacts
  forM_ (M.toList $ stateForView @stores @props @'EventHandlerCode 0) $ \(ty, states) -> do
    art <- js_newArtifact
    forM_ states $ \s -> do
      case s of
        StoreState i -> js_addStoreState art i
        StoreDerivedState i mkCall -> js_addStoreDerivedState art i =<< mkCall
    js_setArtifact artifacts (typeJsKey ty) art

  propsEq <- allEq (Proxy :: Proxy props)
  stateEq <- allEq (Proxy :: Proxy stores)
  View <$> js_createNewCtrlView name renderCb artifacts propsEq stateEq
{-# NOINLINE mkControllerView #-}

exportReactViewToJavaScript :: forall (props :: [*]) handler. (ExportViewProps props handler)
    => View props -> IO JSVal
exportReactViewToJavaScript (View v) = do
  (_callbackToRelease, wrappedCb) <- exportNewViewToJs v (getProps @props @handler)
  return wrappedCb

callbackRenderingView :: forall (props :: [*]) handler. (ExportViewProps props handler)
    => JSString -> View props -> PropertyOrHandler handler
callbackRenderingView name (View v) = CallbackPropertyReturningNewView name v (getProps @props @handler)

getProps :: forall (props :: [*]) handler. (ExportViewProps props handler)
    => JSArray -> IO NewJsProps
getProps arr = do
  props <- js_newEmptyPropList
  applyViewPropsFromArray @props @handler arr 0 props
  return props


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Transform a controller view handler to a raw handler.
runViewHandler :: ReactThis state props -> ViewEventHandler -> IO ()
runViewHandler _ = mapM_ executeAction

-- | Transform a stateful view event handler to a raw event handler.
runStateViewHandler :: (Typeable state)
                    => ReactThis state props -> StatefulViewEventHandler state -> IO ()
runStateViewHandler this handler = do
  st <- js_ReactGetState this >>= unsafeDerefExport "runStateViewHandler"

  let (actions, mNewState) = handler st

  case mNewState of
    Nothing -> return ()
    Just newState -> do
      newStateRef <- export newState
      js_ReactUpdateAndReleaseState this newStateRef

  executeAction `mapM_` actions

getProp :: Typeable a => NewJsProps -> Int -> IO a
getProp p i = js_getPropFromList p i >>= unsafeDerefExport "getProp"

pushProp :: Typeable a => a -> NewJsProps -> IO ()
pushProp val props = do
  valE <- export $! val -- this will be released in the lifecycle callbacks of the class
  js_pushProp props valE

findFromState :: Typeable a => Int -> JsState -> IO a
findFromState i s = js_findFromState i s >>= unsafeDerefExport "findFromState"

newtype Artifacts = Artifacts JSVal
instance IsJSVal Artifacts

newtype Artifact = Artifact JSVal
instance IsJSVal Artifact

newtype RenderCbArg = RenderCbArg JSVal
instance IsJSVal RenderCbArg

#ifdef __GHCJS__

foreign import javascript unsafe
  "$1.input"
  js_getDeriveInput :: JSVal -> IO (Export store)

foreign import javascript unsafe
  "$1.output = $2"
  js_setDeriveOutput :: JSVal -> Export a -> IO ()

foreign import javascript unsafe
    "hsreact$mk_new_view($1, $2, $3)"
    js_createNewView
        :: JSString
        -> Callback (JSVal -> JSVal -> IO ())
        -> Callback (JSVal -> JSVal -> IO JSVal)
        -> IO (ReactViewRef props)

foreign import javascript unsafe
    "hsreact$mk_new_stateful_view($1, $2, $3, $4, $5)"
    js_createNewStatefulView
        :: JSString
        -> Export state
        -> Callback (JSVal -> JSVal -> IO ())
        -> Callback (JSVal -> JSVal -> IO JSVal)
        -> Callback (JSVal -> JSVal -> IO JSVal)
        -> IO (ReactViewRef props)

foreign import javascript unsafe
  "hsreact$mk_new_ctrl_view($1, $2, $3, $4, $5)"
  js_createNewCtrlView
    :: JSString
    -> Callback (JSVal -> JSVal -> IO ())
    -> Artifacts
    -> Callback (JSVal -> JSVal -> IO JSVal)
    -> Callback (JSVal -> JSVal -> IO JSVal)
    -> IO (ReactViewRef props)

foreign import javascript unsafe
  "[]"
  js_newEmptyPropList :: IO NewJsProps

foreign import javascript unsafe
  "$1['props'].hs"
  js_PropsList :: ReactThis state props -> IO NewJsProps

foreign import javascript unsafe
  "$1['state'].hs"
  js_NewStateDict :: ReactThis state props -> IO JsState

foreign import javascript unsafe
  "$1[$2]"
  js_getPropFromList :: NewJsProps -> Int -> IO (Export a)

foreign import javascript unsafe
  "$1.push($2)"
  js_pushProp :: NewJsProps -> Export a -> IO ()

foreign import javascript unsafe
  "$2[$1]"
  js_findFromState :: Int -> JsState -> IO (Export a)

foreign import javascript unsafe
  "{}"
  js_emptyArtifacts :: IO Artifacts

foreign import javascript unsafe
  "[]"
  js_newArtifact :: IO Artifact

foreign import javascript unsafe
  "$1.push({i: $2})"
  js_addStoreState :: Artifact -> Int -> IO ()

foreign import javascript unsafe
  "$1.push({i: $2, call: $3})"
  js_addStoreDerivedState :: Artifact -> Int -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "$1[$2] = $3"
  js_setArtifact :: Artifacts -> JSString -> Artifact -> IO ()

foreign import javascript unsafe
  "$1['state'].hs"
  js_ReactGetState :: ReactThis state props -> IO (Export state)

foreign import javascript unsafe
  "$1._updateAndReleaseState($2)"
  js_ReactUpdateAndReleaseState :: ReactThis state props -> Export state -> IO ()

foreign import javascript unsafe
  "$1.newCallbacks = $2; $1.elem = $3;"
  js_RenderCbSetResults :: RenderCbArg -> JSVal -> ReactElementRef -> IO ()

#else

js_getDeriveInput :: JSVal -> IO (Export store)
js_getDeriveInput _ = error "js_getDeriveInput only works with GHCJS"

js_setDeriveOutput :: JSVal -> Export a -> IO ()
js_setDeriveOutput _ _ = error "js_setDeriveOutput only works with GHCJS"

js_createNewView
    :: JSString
    -> Callback (JSVal -> JSVal -> IO ())
    -> Callback (JSVal -> JSVal -> IO JSVal)
    -> IO (ReactViewRef props)
js_createNewView _ _ _ = error "js_createNewView only works with GHCJS"

js_createNewStatefulView
    :: JSString
    -> Export state
    -> Callback (JSVal -> JSVal -> IO ())
    -> Callback (JSVal -> JSVal -> IO JSVal)
    -> Callback (JSVal -> JSVal -> IO JSVal)
    -> IO (ReactViewRef props)
js_createNewStatefulView _ _ _ _ _ = error "js_createNewStatefulView only works with GHCJS"

js_createNewCtrlView
    :: JSString
    -> Callback (JSVal -> JSVal -> IO ())
    -> Artifacts
    -> Callback (JSVal -> JSVal -> IO JSVal)
    -> Callback (JSVal -> JSVal -> IO JSVal)
    -> IO (ReactViewRef props)
js_createNewCtrlView _ _ _ _ _ = error "js_createNewCtrlView only works with GHCJS"

js_newEmptyPropList :: IO NewJsProps
js_newEmptyPropList = error "js_newEmptyPropList only works with GHCJS"

js_PropsList :: ReactThis state props -> IO NewJsProps
js_PropsList _ = error "js_PropsList only works with GHCJS"

js_NewStateDict :: ReactThis state props -> IO JsState
js_NewStateDict _ = error "js_NewStateDict only works with GHCJS"

js_getPropFromList :: NewJsProps -> Int -> IO (Export a)
js_getPropFromList _ _ = error "js_getPropFromList only works with GHCJS"

js_pushProp :: NewJsProps -> Export a -> IO ()
js_pushProp _ _ = error "js_pushProp only works with GHCJS"

js_findFromState :: Int -> JsState -> IO (Export a)
js_findFromState _ _ = error "js_findFromState only works with GHCJS"

js_emptyArtifacts :: IO Artifacts
js_emptyArtifacts = error "js_emptyArtifacts only works with GHCJS"

js_newArtifact :: IO Artifact
js_newArtifact = error "js_newArtifact only works with GHCJS"

js_addStoreState :: Artifact -> Int -> IO ()
js_addStoreState _ _ = error "js_addStoreState only works with GHCJS"

js_addStoreDerivedState :: Artifact -> Int -> Callback (JSVal -> IO ()) -> IO ()
js_addStoreDerivedState _ _ _ = error "js_addStoreDerivedState only works with GHCJS"

js_setArtifact :: Artifacts -> JSString -> Artifact -> IO ()
js_setArtifact _ _ _ = error "js_setArtifact only works with GHCJS"

js_ReactGetState :: ReactThis state props -> IO (Export state)
js_ReactGetState _ = error "js_ReactGetState only works with GHCJS"

js_ReactUpdateAndReleaseState :: ReactThis state props -> Export state -> IO ()
js_ReactUpdateAndReleaseState _ _ = error "js_ReactUpdateAndReleaseState only works with GHCJS"

js_RenderCbSetResults :: RenderCbArg -> JSVal -> ReactElementRef -> IO ()
js_RenderCbSetResults _ _ _ = error "js_RenderCbSetResults only works with GHCJS"

#endif
