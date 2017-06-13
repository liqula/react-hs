-- | Internal module containing the view definitions
{-# LANGUAGE CPP, UndecidableInstances, TypeApplications, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module React.Flux.Outdated
  ( SomeStoreAction(..)
  , ReactView
  , ReactViewKey(..)
  , ViewEventHandler
  , StatefulViewEventHandler
  , exportViewToJavaScript
  , view
  , viewWithKey
  , viewWithIKey
  , viewWithSKey
  , childrenPassedToView
  , ArgumentsToProps
  , ReturnProps(..)
  , callbackView
  , callbackViewWithProps
  , defineLifecycleView
  , lifecycleConfig
  , LifecycleViewConfig(..)
  , LPropsAndState(..)
  , LDOM(..)
  , LSetStateFn
  , reactRender
  , reactRenderToString
  ) where

import Control.Monad.Writer
import Data.Typeable
import Control.DeepSeq
import Data.Text

import React.Flux.Store
import React.Flux.Internal
import React.Flux.Views (ViewEventHandler, StatefulViewEventHandler)
import React.Flux.DOM (div_)

import System.IO.Unsafe (unsafePerformIO)
import JavaScript.Array
import GHCJS.Foreign (jsNull)
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Export
import GHCJS.Types (JSVal, IsJSVal, nullRef, jsval)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(..))
import qualified JavaScript.Array as JSA


-- | A view is conceptually a rendering function from @props@ and some internal state to a tree of elements.  The function
-- receives a value of type @props@ from its parent in the virtual DOM.  Additionally, the rendering
-- function can depend on some internal state or store data.  Based on the @props@ and the internal
-- state, the rendering function produces a virtual tree of elements which React then reconciles
-- with the browser DOM.
--
-- This module supports 3 kinds of views.  All of the views provided by this module are pure, in the
-- sense that the rendering function and event handlers cannot perform any IO.  All IO occurs inside
-- the 'transform' function of a store.
--
-- Due to React limitations (see <https://github.com/facebook/react/issues/2127 issue2127>), React
-- views must have a single top-level element.  If your haskell code returns multiple top-level
-- elements, react-flux will wrap them in a container @div@.  You should not rely on this and instead
-- make sure each view returns only a single top-level element (such as @todoItem@ below returning only
-- a single @li@ element).
newtype ReactView props = ReactView { reactView :: ReactViewRef props }


-- | Transform a stateful view event handler to a raw event handler
runStateViewHandler :: (Typeable state, NFData state)
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


---------------------------------------------------------------------------------------------------
--- Various GHCJS only utilities
---------------------------------------------------------------------------------------------------

newtype RenderCbArg = RenderCbArg JSVal
instance IsJSVal RenderCbArg

mkRenderCallback :: Typeable props
                 => (ReactThis state props -> IO state) -- ^ parse state
                 -> (ReactThis state props -> eventHandler -> IO ()) -- ^ execute event args
                 -> (state -> props -> IO (ReactElementM eventHandler ())) -- ^ renderer
                 -> IO (Callback (JSVal -> JSVal -> IO ()))
mkRenderCallback parseState runHandler render = syncCallback2 ContinueAsync $ \thisRef argRef -> do
    let this = ReactThis thisRef
        arg = RenderCbArg argRef
    state <- parseState this
    props <- js_ReactGetProps this >>= unsafeDerefExport "mkRenderCallback"
    node <- render state props
    (element, evtCallbacks) <- mkReactElement (runHandler this) this node

    evtCallbacksRef <- toJSVal evtCallbacks
    js_RenderCbSetResults arg evtCallbacksRef element


----------------------------------------------------------------------------------------------------
--- Element creation for views
----------------------------------------------------------------------------------------------------

-- | Create an element from a view.  I suggest you make a combinator for each of your views, similar
-- to the examples above such as @todoItem_@.
view :: Typeable props
     => ReactView props -- ^ the view
     -> props -- ^ the properties to pass into the instance of this view
     -> ReactElementM eventHandler a -- ^ The children of the element
     -> ReactElementM eventHandler a
view rc props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ViewElement (reactView rc) Nothing props childEl

-- | Keys in React can either be strings or integers
class ReactViewKey key where
    toKeyRef :: key -> JSVal
instance ReactViewKey String where
    toKeyRef = pToJSVal

instance ReactViewKey Int where
    toKeyRef = pToJSVal

-- | A deprecated way to create a view with a key which has problems when OverloadedStrings is
-- active.  Use 'viewWithSKey' or 'viewWithIKey' instead.
viewWithKey :: (Typeable props, ReactViewKey key)
            => ReactView props -- ^ the view
            -> key -- ^ A value unique within the siblings of this element
            -> props -- ^ The properties to pass to the view instance
            -> ReactElementM eventHandler a -- ^ The children of the view
            -> ReactElementM eventHandler a
viewWithKey rc key props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ViewElement (reactView rc) (Just $ toKeyRef key) props childEl

-- | Create an element from a view, and also pass in a string key property for the instance.  Key
-- properties speed up the <https://facebook.github.io/react/docs/reconciliation.html reconciliation>
-- of the virtual DOM with the DOM.  The key does not need to be globally unqiue, it only needs to
-- be unique within the siblings of an element.
viewWithSKey :: Typeable props
             => ReactView props -- ^ the view
             -> JSString -- ^ The key, a value unique within the siblings of this element
             -> props -- ^ The properties to pass to the view instance
             -> ReactElementM eventHandler a -- ^ The children of the view
             -> ReactElementM eventHandler a
viewWithSKey rc key props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ViewElement (reactView rc) (Just $ pToJSVal key) props childEl

-- | Similar to 'viewWithSKey', but with an integer key instead of a string key.
viewWithIKey :: Typeable props
             => ReactView props -- ^ the view
             -> Int -- ^ The key, a value unique within the siblings of this element
             -> props -- ^ The properties to pass to the view instance
             -> ReactElementM eventHandler a -- ^ The children of the view
             -> ReactElementM eventHandler a
viewWithIKey rc key props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ViewElement (reactView rc) (Just $ pToJSVal key) props childEl

-- | A class which is used to implement <https://wiki.haskell.org/Varargs variable argument functions>.
-- These variable argument functions are used to convert from a JavaScript
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/arguments arguments array>
-- to a Haskell value of type @props@.
--
-- Any function where each argument implements 'FromJSVal' and the result is 'ReturnProps' is an
-- instance of this class.  Entries from the JavaScript arguments array are matched one-by-one to
-- the arguments before 'ReturnProps' value.  If the Haskell function has more parameters than the
-- javascript @arguments@ object, a javascript null is used for the conversion.  Since the 'Maybe'
-- instance of 'FromJSVal' converts null references to 'Nothing', you can exploit this to handle
-- arguments not given to the JavaScript function.
class ArgumentsToProps props a | a -> props where
    returnViewFromArguments :: JSArray -> Int -> a -> IO props

-- | A type needed to make GHC happy when solving for instances of 'ArgumentsToProps'.
newtype ReturnProps props = ReturnProps props

instance ArgumentsToProps props (ReturnProps props) where
    returnViewFromArguments _ _ (ReturnProps v) = return v

instance (FromJSVal a, ArgumentsToProps props b) => ArgumentsToProps props (a -> b) where
    returnViewFromArguments args k f = do
        ma <- fromJSVal $ if k >= JSA.length args then nullRef else JSA.index k args
        a <- maybe (error "Unable to decode callback argument") return ma
        returnViewFromArguments args (k+1) $ f a

-- | Export a Haskell view to a JavaScript function.  This allows you to embed a Haskell react-flux
-- application into a larger existing JavaScript React application.  If you want to use JavaScript
-- classes in your Haskell application, you should instead use 'React.Flux.Combinators.foreign_' and 'foreignClass'.
--
-- The way this works is as follows:
--
-- 1. You create a Haskell function which translates the javascript arguments of into a Haskell
-- value of type @ReturnProps props@.  This is a variable-argument function using the 'ArgumentsToProps' class.
-- For example,
--
--       @
--       data MyProps = MyProps { theInt :: Int, theString :: String }
--       myArgsToProps :: Int -> String -> ReturnProps MyProps
--       myArgsToProps i s = ReturnProps $ MyProps i s
--       @
--
-- 2. You create a view which receives these properties and renders itself.  This view will not
-- receive any children.
--
--       @
--       myView :: ReactView MyProps
--       myView = defineView "my view" $ \\myProps -> ...
--       @
--
-- 3. You can then use 'exportViewToJavaScript' to create a JavaScript function.  When this
-- JavaScript function is executed, the JavaScript arguments are converted to the props,
-- the view is rendered using the props, and the resulting React element is returned from the
-- JavaScript function.
--
--       @
--       foreign import javascript unsafe
--           "window[\'myHaskellView\'] = $1;"
--           js_setMyView :: JSVal -> IO ()
--
--       exportMyView :: IO ()
--       exportMyView = exportViewToJavaScript myView myArgsToProps >>= js_setMyView
--       @
--
--       @exportMyView@ should be called from your main function.  After executing @exportMyView@,
--       the @window.myHaskellView@ property will be a javascript function.
--
-- 4. Call the javascript function with two arguments to return a React element which can be used
-- in a JavaScript React class rendering function.
--
--       @
--       var myJsView = React.createClass({
--           render: function() {
--               return \<div\>{window.myHaskellView(5, "Hello World")}\</div\>;
--           }
--       };
--       @
exportViewToJavaScript :: (Typeable props, ArgumentsToProps props func) => ReactView props -> func -> IO JSVal
exportViewToJavaScript v func = do
    (_callbackToRelease, wrappedCb) <- exportViewToJs (reactView v) (\arr -> returnViewFromArguments arr 0 func)
    return wrappedCb

-- | Create a zero-argument callback property.  When this callback function is executed, it
-- will render the given view and return the resulting React element.  If you need to
-- create a callback which expects arguments, use 'callbackViewWithProps' instead.
callbackView :: JSString -> ReactView () -> PropertyOrHandler handler
callbackView name v = CallbackPropertyReturningView name (const $ return ()) (reactView v)

-- | Create a callback that when called will render a view.  This is useful for interacting with third-party React classes that expect
-- a property which is a function which when called returns a React element.   The way this works is
-- as follows:
--
-- 1. You create a Haskell function which translates the javascript arguments of the callback into a Haskell
-- value of type @ReturnProps props@.  This is a variable-argument function using the 'ArgumentsToProps' class.
-- For example,
--
--       @
--       data MyProps = MyProps { theInt :: Int, theString :: String }
--       myArgsToProps :: Int -> String -> ReturnProps MyProps
--       myArgsToProps i s = ReturnProps $ MyProps i s
--       @
--
-- 2. You create a view which receives these properties and renders itself.  This view will not
-- receive any children.
--
--       @
--       myView :: ReactView MyProps
--       mYView = defineView "my view" $ \\myProps -> ...
--       @
--
-- 3. You can then use 'callbackViewWithProps' to create a property which is a JavaScript function.
-- When this JavaScript function is executed, the JavaScript arguments are converted to the props,
-- the view is rendered using the props, and the resulting React element is returned from the
-- JavaScript function.
--
--       @
--       someOtherView :: ReactView ()
--       someOtherView = defineView "some other view" $ \\() ->
--           div_ $
--              foreignClass_ "theForeginThing"
--                  [ callbackViewWithProps "the_propname_to_pass_to_theForeignThing" myView myArgsToProps
--                  , "hello" $= "world"
--                  ] mempty
--      @
--
--      @theForeignThing@ React class will receive a property called
--      @the_propname_to_pass_to_theForeignThing@.  The value of this property is a JavaScript
--      function which when executed will convert the arguments to @props@, render the view, and
--      return the resulting React element.
callbackViewWithProps :: (Typeable props, ArgumentsToProps props func) => JSString -> ReactView props -> func -> PropertyOrHandler handler
callbackViewWithProps name v func = CallbackPropertyReturningView name (\arr -> returnViewFromArguments arr 0 func) (reactView v)

type HTMLElement = JSVal

-- | Actions to access the current properties and state.
data LPropsAndState props state = LPropsAndState
  { lGetProps :: IO props
  , lGetState :: IO state
  }

-- | Obtain the browser DOM element for either the component as a whole with 'lThis' or for various
-- nodes with a given <https://facebook.github.io/react/docs/more-about-refs.html ref> property with
-- 'lRef'.
data LDOM = LDOM
  { lThis :: IO HTMLElement
  , lRef :: JSString -> IO HTMLElement
  }

-- | Set the state of the class.
type LSetStateFn state = state -> IO ()

-- | The class rendering function, together with optional callbacks for the various lifecycle
-- events.  As mentioned above, care must be taken in each callback to write only IO that will not
-- block.
data LifecycleViewConfig props state = LifecycleViewConfig
  { lRender :: state -> props -> ReactElementM (StatefulViewEventHandler state) ()
  , lComponentWillMount :: Maybe (LPropsAndState props state -> LSetStateFn state -> IO ())
  , lComponentDidMount :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> IO ())
  -- | Receives the new props as an argument.
  , lComponentWillReceiveProps :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> props -> IO ())
  -- | Receives the new props and state as arguments.  The current props and state can be accessed using
  -- 'LPropsAndState'.
  , lComponentWillUpdate :: Maybe (LPropsAndState props state -> LDOM -> props -> state -> IO ())
  -- | Receives the old props and state as arguments.  The current props and state can be accessed
  -- using 'LPropsAndState'
  , lComponentDidUpdate :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> props -> state -> IO ())
  , lComponentWillUnmount :: Maybe (LPropsAndState props state -> LDOM -> IO ())
  }

-- | A default configuration, which does not specify any lifecycle events.  You should start with
-- this and override the functions you need.
lifecycleConfig :: LifecycleViewConfig props state
lifecycleConfig = LifecycleViewConfig
    { lRender = \_ _ -> div_ mempty
    , lComponentWillMount = Nothing
    , lComponentDidMount = Nothing
    , lComponentWillReceiveProps = Nothing
    , lComponentWillUpdate = Nothing
    , lComponentDidUpdate = Nothing
    , lComponentWillUnmount = Nothing
    }

-- | Create a lifecycle view from the given configuration.
--
-- >myView :: ReactView String
-- >myVew = defineLifecycleView "my view" (10 :: Int) lifecycleConfig
-- >            { lRender = \state props -> ...
-- >            , lComponentWillMount = \propsAndState setStateFn -> ...
-- >            }
{-# NOINLINE defineLifecycleView #-}
defineLifecycleView :: forall props state . (Typeable props, Eq props, Typeable state, NFData state, Eq state)
              => String -> state -> LifecycleViewConfig props state -> ReactView props

defineLifecycleView name initialState cfg = unsafePerformIO $ do
    initialRef <- export initialState

    let render state props = return $ lRender cfg state props
    renderCb <- mkRenderCallback (js_ReactGetState >=> unsafeDerefExport "defineLifecycleView.renderCb")
                      runStateViewHandler render

    let dom this = LDOM { lThis = js_ReactFindDOMNode this
                        , lRef = \r -> js_ReactGetRef this r
                        }

        setStateFn this s = export s >>= js_ReactUpdateAndReleaseState this

    willMountCb <- mkLCallback1 (lComponentWillMount cfg) $ \f this ->
        f (setStateFn this)

    didMountCb <- mkLCallback1 (lComponentDidMount cfg) $ \f this ->
        f (dom this) (setStateFn this)

    willRecvPropsCb <- mkLCallback2 (lComponentWillReceiveProps cfg) $ \f this newPropsE -> do
        newProps <- unsafeDerefExport "defineLifecycleView.willRecvProps" $ fakeJSValToExport newPropsE
        f (dom this) (setStateFn this) newProps

    willUpdateCb <- mkLCallback2 (lComponentWillUpdate cfg) $ \f this argRef -> do
        let arg = ReactThis argRef
        nextProps <- js_ReactGetProps arg >>= unsafeDerefExport "defineLifecycleView.willUpdateCb.Props"
        nextState <- js_ReactGetState arg >>= unsafeDerefExport "defineLifecycleView.willUpdateCb.State"
        f (dom this) nextProps nextState

    didUpdateCb <- mkLCallback2 (lComponentDidUpdate cfg) $ \f this argRef -> do
        let arg = ReactThis argRef
        oldProps <- js_ReactGetProps arg >>= unsafeDerefExport "defineLifecycleView.didUpdateCb.Props"
        oldState <- js_ReactGetState arg >>= unsafeDerefExport "defineLifecycleView.didUpdateCb.State"
        f (dom this) (setStateFn this) oldProps oldState

    willUnmountCb <- mkLCallback1 (lComponentWillUnmount cfg) $ \f this ->
        f (dom this)

    compState <- singleEq (Proxy :: Proxy state)
    compProps <- singleEq (Proxy :: Proxy props)

    -- willMountCbRef <- toJSVal willMountCb
    -- didMountCbRef <- toJSVal didMountCb
    -- willRecvPropsCbRef <- toJSVal willRecvPropsCb
    -- willUpdateCbRef  <- toJSVal willUpdateCb
    -- didUpdateCbRef   <- toJSVal didUpdateCb
    -- willUnmountCbRef <- toJSVal willUnmountCb

    ReactView <$> js_makeLifecycleView (toJSString name) initialRef
      renderCb willMountCb didMountCb willRecvPropsCb willUpdateCb didUpdateCb willUnmountCb compProps compState

mkLCallback1 :: (Typeable props, Typeable state)
             => Maybe (LPropsAndState props state -> f)
             -> (f -> ReactThis state props -> IO ())
             -> IO JSVal
mkLCallback1 Nothing _ = return jsNull
mkLCallback1 (Just f) c = do
  cb <- syncCallback1 ThrowWouldBlock $ \thisRef -> do
    let this = ReactThis thisRef
        ps = LPropsAndState { lGetProps = js_ReactGetProps this >>= unsafeDerefExport "mkLCallback1.props"
                            , lGetState = js_ReactGetState this >>= unsafeDerefExport "mkLCallback1.state"
                            }
    c (f ps) this
  return $ jsval cb

mkLCallback2 :: (Typeable props, Typeable state)
             => Maybe (LPropsAndState props state -> f)
             -> (f -> ReactThis state props -> JSVal -> IO ())
             -> IO JSVal
mkLCallback2 Nothing _ = return jsNull
mkLCallback2 (Just f) c = do
  cb <- syncCallback2 ThrowWouldBlock $ \thisRef argRef -> do
    let this = ReactThis thisRef
        ps = LPropsAndState { lGetProps = js_ReactGetProps this >>= unsafeDerefExport "mkLCallback2.props"
                            , lGetState = js_ReactGetState this >>= unsafeDerefExport "mkLCallback2.state"
                            }
    c (f ps) this argRef
  return $ jsval cb


----------------------------------------------------------------------------------------------------
-- reactRender has two versions
----------------------------------------------------------------------------------------------------

-- | Render your React application into the DOM.  Use this from your @main@ function, and only in the browser.
-- 'reactRender' only works when compiled with GHCJS (not GHC), because we rely on the React javascript code
-- to actually perform the rendering.
reactRender :: Typeable props
            => String -- ^ The ID of the HTML element to render the application into.
                      -- (This string is passed to @document.getElementById@)
            -> ReactView props -- ^ A single instance of this view is created
            -> props -- ^ the properties to pass to the view
            -> IO ()
reactRender htmlId rc props = do
    (e, _) <- mkReactElement id (ReactThis nullRef) $ view rc props mempty
    js_ReactRender e (toJSString htmlId)

-- | Render your React application to a string using either @ReactDOMServer.renderToString@ if the first
-- argument is false or @ReactDOMServer.renderToStaticMarkup@ if the first argument is true.
-- Use this only on the server when running with node.
-- 'reactRenderToString' only works when compiled with GHCJS (not GHC), because we rely on the React javascript code
-- to actually perform the rendering.
--
-- If you are interested in isomorphic React, I suggest instead of using 'reactRenderToString' you use
-- 'exportViewToJavaScript' and then write a small top-level JavaScript view which can then integrate with
-- all the usual isomorphic React tools.
reactRenderToString :: Typeable props
                    => Bool -- ^ Render to static markup?  If true, this won't create extra DOM attributes
                            -- that React uses internally.
                    -> ReactView props -- ^ A single instance of this view is created
                    -> props -- ^ the properties to pass to the view
                    -> IO Text
reactRenderToString includeStatic rc props = do
    (e, _) <- mkReactElement id (ReactThis nullRef) $ view rc props mempty
    sRef <- (if includeStatic then js_ReactRenderStaticMarkup else js_ReactRenderToString) e
    --return sRef
    --return $ JSS.unpack sRef
    mtxt <- fromJSVal sRef
    maybe (error "Unable to convert string return to Text") return mtxt

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1['state'].hs"
    js_ReactGetState :: ReactThis state props -> IO (Export state)

foreign import javascript unsafe
    "$1['props'].hs"
    js_ReactGetProps :: ReactThis state props -> IO (Export props)

foreign import javascript unsafe
    "$1._updateAndReleaseState($2)"
    js_ReactUpdateAndReleaseState :: ReactThis state props -> Export state -> IO ()

foreign import javascript unsafe
    "$1.newCallbacks = $2; $1.elem = $3;"
    js_RenderCbSetResults :: RenderCbArg -> JSVal -> ReactElementRef -> IO ()

foreign import javascript unsafe
    "hsreact$mk_lifecycle_view($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)"
    js_makeLifecycleView :: JSString -> Export state -> Callback (JSVal -> JSVal -> IO ())
                         -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal
                         -> Callback (JSVal -> JSVal -> IO JSVal)
                         -> Callback (JSVal -> JSVal -> IO JSVal)
                         -> IO (ReactViewRef props)

-- React 0.13 has React.findDOMNode, while 0.14 moves it to ReactDOM.findDOMNode.  Also, 0.14
-- does not need to call findDOMNode on refs.
foreign import javascript unsafe
    "typeof ReactDOM === 'object' ? ReactDOM['findDOMNode']($1) : React['findDOMNode']($1)"
    js_ReactFindDOMNode :: ReactThis state props -> IO JSVal

foreign import javascript unsafe
    "typeof ReactDOM === 'object' ? $1['refs'][$2] : React['findDOMNode']($1['refs'][$2])"
    js_ReactGetRef :: ReactThis state props -> JSString -> IO JSVal

foreign import javascript unsafe
    "(typeof ReactDOM === 'object' ? ReactDOM : React)['render']($1, document.getElementById($2))"
    js_ReactRender :: ReactElementRef -> JSString -> IO ()

foreign import javascript unsafe
    "(typeof ReactDOMServer === 'object' ? ReactDOMServer : (typeof ReactDOM === 'object' ? ReactDOM : React))['renderToString']($1)"
    js_ReactRenderToString :: ReactElementRef -> IO JSVal

foreign import javascript unsafe
    "(typeof ReactDOMServer === 'object' ? ReactDOMServer : (typeof ReactDOM === 'object' ? ReactDOM : React))['renderToStaticMarkup']($1)"
    js_ReactRenderStaticMarkup :: ReactElementRef -> IO JSVal

#else

js_ReactGetState :: ReactThis state props -> IO (Export state)
js_ReactGetState _ = error "js_ReactGetState only works with GHCJS"

js_ReactGetProps :: ReactThis state props -> IO (Export props)
js_ReactGetProps _ = error "js_ReactGetProps only works with GHCJS"

js_ReactUpdateAndReleaseState :: ReactThis state props -> Export state -> IO ()
js_ReactUpdateAndReleaseState _ _ = error "js_ReactUpdateAndReleaseState only works with GHCJS"

js_RenderCbSetResults :: RenderCbArg -> JSVal -> ReactElementRef -> IO ()
js_RenderCbSetResults _ _ _ = error "js_RenderCbSetResults only works with GHCJS"

js_makeLifecycleView :: JSString -> Export state -> Callback (JSVal -> JSVal -> IO ())
                     -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal
                     -> Callback (JSVal -> JSVal -> IO JSVal)
                     -> Callback (JSVal -> JSVal -> IO JSVal)
                     -> IO (ReactViewRef props)
js_makeLifecycleView _ _ _ _ _ _ _ _ _ _ _ = error "js_makeLifecycleView only works with GHCJS"

-- React 0.13 has React.findDOMNode, while 0.14 moves it to ReactDOM.findDOMNode.  Also, 0.14
-- does not need to call findDOMNode on refs.
js_ReactFindDOMNode :: ReactThis state props -> IO JSVal
js_ReactFindDOMNode _ = error "js_ReactFindDOMNode only works with GHCJS"

js_ReactGetRef :: ReactThis state props -> JSString -> IO JSVal
js_ReactGetRef _ _ = error "js_ReactGetRef only works with GHCJS"

js_ReactRender :: ReactElementRef -> JSString -> IO ()
js_ReactRender _ _ = error "js_ReactRender only works with GHCJS"

js_ReactRenderToString :: ReactElementRef -> IO JSVal
js_ReactRenderToString _ = error "js_ReactRenderToString only works with GHCJS"

js_ReactRenderStaticMarkup :: ReactElementRef -> IO JSVal
js_ReactRenderStaticMarkup _ = error "js_ReactRenderStaticMarkup only works with GHCJS"

#endif
