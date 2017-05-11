-- | A binding to <https://facebook.github.io/react/index.html React> based on the
-- <https://facebook.github.io/flux/docs/overview.html Flux> design.  The flux design pushes state
-- and complicated logic out of the view, allowing the rendering functions and event handlers to be
-- pure Haskell functions.  When combined with React's composable components and the one-way flow of
-- data, React, Flux, and GHCJS work very well together.
--
-- __Prerequisites__: This module assumes you are familiar with the basics of React and Flux.  From
-- the <https://facebook.github.io/react/docs/tutorial.html React documentation>, you should read at
-- least \"Tutorial\", \"Displaying Data\", \"Multiple Components\", and \"Forms\".  Note that
-- instead of JSX we use a Writer monad, but it functions very similarly so the examples in the
-- React documentation are very similar to how you will write code using this module.  The other
-- React documentation you can skim, the Haddocks below link to specific sections of the React
-- documentation when needed.  Finally, you should read the
-- <https://facebook.github.io/flux/docs/overview.html Flux overview>, in particular the central
-- idea of one-way flow of data from actions to stores to views which produce actions.
--
-- __Organization__: Briefly, you should create one module to contain the dispatcher, one module for
-- each store, and modules for the view definitions.  These are then imported into a Main module,
-- which calls 'reactRender' and initializes any AJAX load calls to the backend. The source package
-- contains some <https://bitbucket.org/wuzzeb/react-flux/src/tip/example/ example applications>.
--
-- __Web Deployment__: 'reactRender' is used to render your application into the DOM.
-- Care has been taken to make sure closure with ADVANCED_OPTIMIZATIONS correctly
-- minimizes a react-flux application.  No externs are needed, instead all you need to do is
-- protect the @React@ variable (and @ReactDOM@ if you are using version >= 0.14).  The TODO example
-- does this as follows:
--
-- >(function(global, React, ReactDOM) {
-- >contents of all.js
-- >})(window, window['React'], window['ReactDOM']);
--
-- __Node Deployment__: 'reactRenderToString' is used to render the application to a string when
-- running in node (not the browser).  To execute with node, you need to get @global.React@ and
-- @global.ReactDOMServer@  before executing all.js.  The TODO example application does this by
-- creating a file @run-in-node.js@ with the contents
--
-- >React = require("react");
-- >ReactDOMServer = require("react-dom/server");
-- >require("../../js-build/install-root/bin/todo-node.jsexe/all.js");
--
-- __React Native__: This module also works with <https://facebook.github.io/react-native/ React-Native>
-- to create a standalone native applications.  When combined with <http://electron.atom.io/ electron>,
-- you can even create standalone desktop applications.  The workflow is to use 'reactRender' the
-- same as web deployment but use the resulting JavaScript file in react-native and/or electron.
-- <https://github.com/jyrimatti/hseverywhere Jyrimatti has an example using react-native>.
--
-- __Testing__:  I use the following approach to test my react-flux application.  First, I use unit
-- testing to test the dispatcher and store 'transform' functions.  Since the dispatcher and the
-- store transform are just data manipulation, existing Haskell tools like hspec, QuickCheck,
-- SmallCheck, etc. work well.  Note that stores and 'dispatch' work in GHC and GHCJS, so this unit
-- testing can be done either in GHC or GHCJS. I don't do any unit testing of the views, because any
-- complicated logic in event handlers is moved into the dispatcher and the
-- rendering function is difficult to test in isolation.  Instead, I test the rendering via
-- end-2-end tests using <https://hackage.haskell.org/package/hspec-webdriver hspec-webdriver>.
-- This tests the React frontend against the real backend and hspec-webdriver has many utilities for
-- easily checking that the DOM is what you expect.  I have found this much easier than trying to
-- unit test each view individually, and you can still obtain the same coverage for equal effort.
-- The file <https://bitbucket.org/wuzzeb/react-flux/src/tip/test/spec/TodoSpec.hs test\/spec\/TodoSpec.hs>
-- in the source code contains a hspec-webdriver test for the TODO example application.

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-} -- ArgumentsToProps is exported twice, once by React.Flux.PropertiesAndEvents and once here

module React.Flux (
  -- * Dispatcher
  -- $dispatcher

  -- * Stores
    StoreData(..)
  , registerInitialStore
  , SomeStoreAction
  , someStoreAction
  , executeAction
  , transformStore
  , readStoreData

  -- * Views
  , ViewEventHandler
  , View
  , ViewPropsToElement
  , ViewProps
  , mkView
  , StatefulViewEventHandler
  , mkStatefulView
  , view_
  , StoreArg
  , StoreField
  , UnoverlapAllEq
  , ControllerViewToElement
  , HasField(..)
  , mkControllerView

  -- * Elements
  , ReactElement
  , ReactElementM(..)
  , elemString
  , elemText
  , elemJSString
  , elemShow
  , foreignClass
  , rawJsRendering
  , transHandler
  , liftViewToStateHandler
  , callbackRenderingView
  , module React.Flux.DOM
  , module React.Flux.PropertiesAndEvents
  , module React.Flux.Combinators

  -- * Main
  , reactRenderView
  , reactRenderViewToString
  , exportReactViewToJavaScript

  -- * Performance
  -- $performance
) where

import React.Flux.Views
import React.Flux.DOM
import React.Flux.Internal
import React.Flux.PropertiesAndEvents
import React.Flux.Combinators
import React.Flux.Store
import Data.Text (Text)

import GHCJS.Types (nullRef, JSVal)
import GHCJS.Marshal (fromJSVal)

-- | Render your React application into the DOM.  Use this from your @main@ function, and only in the browser.
-- 'reactRender' only works when compiled with GHCJS (not GHC), because we rely on the React javascript code
-- to actually perform the rendering.
reactRenderView :: JSString -- ^ The ID of the HTML element to render the application into.
                      -- (This string is passed to @document.getElementById@)
                -> View '[] -- ^ A single instance of this view is created
                -> IO ()

-- | Render your React application to a string using either @ReactDOMServer.renderToString@ if the first
-- argument is false or @ReactDOMServer.renderToStaticMarkup@ if the first argument is true.
-- Use this only on the server when running with node.
-- 'reactRenderToString' only works when compiled with GHCJS (not GHC), because we rely on the React javascript code
-- to actually perform the rendering.
--
-- If you are interested in isomorphic React, I suggest instead of using 'reactRenderToString' you use
-- 'exportReactViewToJavaScript' and then write a small top-level JavaScript view which can then integrate with
-- all the usual isomorphic React tools.
reactRenderViewToString :: Bool -- ^ Render to static markup?  If true, this won't create extra DOM attributes
                                -- that React uses internally.
                        -> View '[] -- ^ A single instance of this view is created
                        -> IO Text

reactRenderView htmlId (View rc) = do
  (e, _) <- mkReactElement id (ReactThis nullRef) $ elementToM () $ NewViewElement rc htmlId (const $ return ())
  js_ReactRender e htmlId

reactRenderViewToString includeStatic (View rc) = do
  (e, _) <- mkReactElement id (ReactThis nullRef) $ elementToM () $ NewViewElement rc "main" (const $ return ())
  sRef <- (if includeStatic then js_ReactRenderStaticMarkup else js_ReactRenderToString) e
  mtxt <- fromJSVal sRef
  maybe (error "Unable to convert string return to Text") return mtxt


-- $performance
--
-- React obtains high <https://facebook.github.io/react/docs/advanced-performance.html performance> from two techniques: the
-- <https://facebook.github.io/react/docs/reconciliation.html virtual DOM/reconciliation> and
-- <https://facebook.github.io/react/docs/events.html event handlers> registered on the document.
--
-- __Reconciliation__
--
-- To support fast reconciliation, React uses key properties (set by 'viewWithKey') and a
-- @shouldComponentUpdate@ lifetime class method.  The React documentation on
-- <https://facebook.github.io/react/docs/advanced-performance.html performance and immutable-js> talks
-- about using persistent data structures, which is exactly what Haskell does.  Therefore, we
-- implement a @shouldComponentUpdate@ method which compares if the javascript object representing
-- the Haskell values for the @props@, @state@, and/or @storeData@ have changed.  Thus if you do not
-- modify a Haskell value that is used for the @props@ or @state@ or @storeData@, React will skip
-- re-rendering that view instance.  Note that we are not checking equality, just if the javascript
-- object representing a Haskell object has changed, with some special support for pairs and tuples
-- of size three.
--
-- There is subtle issue: this check only works if the props are not a thunk but are an actual data
-- constructor.  Consider the following
--
-- >data MyStoreData = MyStoreData {
-- >   myA :: !A
-- > , myB :: !B
-- > , myC :: !C
-- > , myD :: !D
-- >} deriving (Show, Typeable)
-- >
-- >myAview :: ReactView A
-- >myAview = defineView ....
-- >
-- >myStoreView :: ReactView ()
-- >myStoreView = defineControllerView "my store" myStore $ \myData () ->
-- >    div_ $ view myAview (myA myData) mempty
-- >    div_ ....
--
-- In @myStoreView@, note that @myA myData@ is passed as the props to @myAview@.  So consider the
-- situtation when say an action changes @C@ but leaves @A@ unchanged.  We would like for the
-- rendering of @myAview@ to be skipped, but unfortunately it will be re-rendered.  The reason is
-- that the props passed to @myAview@ is an unevaluated thunk @myA myData@.  Sure, the @A@
-- constructor has not changed and if the thunk is forced it will return this unchanged @A@ data
-- constructor, but the @shouldComponentUpdate@ test does not do any computation or evaluation, it
-- just checks if the passed in javascript object is the same as it was the last time the view was
-- rendered.  We can fix this by forcing the thunk before passing it to 'view', which I do via bang
-- patterns.  Instead of ever calling 'view' directly from a rendering function, for each
-- 'ReactView' I create a combinator as follows:
--
-- >myAview_ :: A -> ReactElementM handler ()
-- >myAview_ !a = view myAview a mempty
-- >
-- >myStoreView :: ReactView ()
-- >myStoreView = defineControllerView "my store" myStore $ \myData () ->
-- >    div_ $ myAview_ (myA myData)
-- >    div_ ....
--
-- Note the bang pattern on the @a@ parameter to @myAview_@.  What now happens is that the bang pattern
-- forces the thunk @myA myData@ to turn into the @A@ data constructor.  If an action does not edit the @A@ portion
-- of the store data, this will still be represented by the same javascript object as before and
-- React will not re-render the @myAview@.
--
-- Now consider another situtation where you would like a view that takes A and B.
--
-- >myAandBview :: ReactView (A, B)
-- >myAandBview = defineView ....
-- >
-- >myAandBview_ :: A -> B -> ReactElementM handler ()
-- >myAandBview_ !a !b = view myAandBview (a, b) mempty
-- >
-- >myStoreView :: ReactView ()
-- >myStoreView = defineControllerView "my store" myStore $ \myData () ->
-- >    div_ $ myAview_ (myA myData)
-- >    div_ $ myAandBview_ (myA myData) (myB myData)
-- >    div_ ....
--
-- Again, if you have an action that just changes @C@ you would like @myAandBview@ to not be
-- re-rendered.  With the simple javascript object check, it would be re-rendered because the props
-- are a tuple and the Haskell value (and thus javascript object) for the tuple is being recreated each
-- time @myStoreView@ is rendered.  To overcome this obstacle, @react-flux@ contains special code to check pairs
-- and tuples of size three.  If the props are a pair or a tuple of size three, the components of
-- the tuple will be compared to see if they are the same javascript object.  Thus similar to the
-- above we need to make sure each component of the tuple is not a thunk but a data constructor,
-- which happens via the bang patterns in @myAandBview_@.  The end result is that if an action just
-- changes @C@ or @D@ and leaves @A@ and @B@ unchanged, the above code will cause React to not
-- re-render @myAandBview@ because the two components of the pair are forced and are still the same
-- unchanged data value/javascript object.  You can see this in action inside the test suite if you
-- would like an example.
--
-- So far we have been focusing on making sure the new props are not a thunk by forcing it before
-- passing it into 'view'.  But we also need to make sure the initial props are not a thunk.  This
-- is not quite as bad since the check will only fail the next time a re-render occurs and after
-- that everything will be OK so we will still mostly skip re-rendering, but is still a small
-- annoyance.  There are several ways to fix this, but the easiest is to add bang patterns to the
-- definition of @MyStoreData@.  If you scroll up you can see that each member of @MyStoreData@ has
-- a bang pattern.  Thus when an action does change @A@, whatever a new value is set into @myA@, it
-- will not be a thunk but an actual data constructor.  Then the initial props passed into the view
-- will not be a thunk.
--
-- In summary, you should follow these rules:
--
--  1. Use bang patterns on each member in your store data.  In fact, once GHC 8 is released, I
--  plan on turning on the new @StrictData@ extension and then all these bang patterns can be
--  dropped.
--
--  2. Try and keep your view parameters as part of the store that will be unchanged by some
--  actions.  Use tuples of size two or three to combine multiple parts of the store data or even
--  data from multiple stores. (Tuples of larger size could be supported without much effort if
--  required.)
--
--  3. For each view, make a combinator with a underscore suffix which uses bang patterns to force
--  the props before passing it to the 'view' function.
--
-- __Events__
--
-- For events, React registers only global event handlers and also keeps event objects (the object
-- passed to the handlers) in a pool and re-uses them for successive events.  We want to parse this
-- event object lazily so that only properties actually accessed are parsed, but this is a problem
-- because lazy access could occur after the event object is reused.  Instead of making a copy of
-- the event, we use the 'NFData' instance on 'SomeStoreAction' to force the evaluation of the store
-- action(s) resulting from the event.  We therefore compute the action before the event object
-- returns to the React pool, and rely on the type system to prevent the leak of the event object
-- outside the handlers.  Thus, you cannot "cheat" in the 'NFData' instance on your store actions;
-- the event objects dilerbertly do not have a 'NFData' instance, so that you must pull all your
-- required data out of the event object and into an action in order to properly implement 'NFData'.
-- Of course, the easiest way to implement 'NFData' is to derive it with Generic and DeriveAnyClass,
-- as @TodoAction@ does above.

-- $dispatcher
-- The dispatcher is the central hub that manages all data flow in a Flux application.  It has no
-- logic of its own and all it does is distribute actions to stores.  There is no special support
-- for a dispatcher in this module, since it can be easily implemented directly using Haskell
-- functions.  The event handlers registered during rendering are expected to produce a list of 'SomeStoreAction'.
-- The dispatcher therefore consists of Haskell functions which produce these lists of
-- 'SomeStoreAction'.  Note that this list of actions is used instead of @waitFor@ to sequence
-- actions to stores: when dispatching, we wait for the 'transform' of each action to complete
-- before moving to the next action.
--
-- In the todo example application there is only a single store, so the dispatcher just
-- passes along the action to the store.  In a larger application, the dispatcher could have its
-- own actions and produce specific actions for each store.
--
-- >dispatchTodo :: TodoAction -> [SomeStoreAction]
-- >dispatchTodo a = [SomeStoreAction todoStore a]

#ifdef __GHCJS__

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

js_ReactRender :: ReactElementRef -> JSString -> IO ()
js_ReactRender _ _ = error "js_ReactRender only works with GHCJS"

js_ReactRenderToString :: ReactElementRef -> IO JSVal
js_ReactRenderToString _ = error "js_ReactRenderToString only works with GHCJS"

js_ReactRenderStaticMarkup :: ReactElementRef -> IO JSVal
js_ReactRenderStaticMarkup _ = error "js_ReactRenderStaticMarkup only works with GHCJS"

#endif
