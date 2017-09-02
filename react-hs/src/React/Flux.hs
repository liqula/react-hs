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
-- which calls 'reactRender' and initializes any AJAX load calls to the backend.  See the
-- <https://hackage.haskell.org/package/react-examples react-examples package> for practial
-- examples.
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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-} -- ArgumentsToProps is exported twice, once by React.Flux.PropertiesAndEvents and once here

module React.Flux (
  -- * Dispatcher
  -- $dispatcher

  -- * Stores
    StoreData(..)
  , registerInitialStore
  , SomeStoreAction
  , action
  , executeAction
  , transformStore
  , readStoreData

  -- * Views
  , EventHandlerType, EventHandlerCode(..)
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
  , ReactElementM_(..)
  , ReactElementM
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
                -> View () -- ^ A single instance of this view is created
                -> IO ()
reactRenderView htmlId (View rc) = do
  let element = elementToM () $ NewViewElement rc htmlId (pushProp ())
  (e, _) <- mkReactElement @'EventHandlerCode (const $ pure ()) (ReactThis nullRef) element
  js_ReactRender e htmlId

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
                        -> View () -- ^ A single instance of this view is created
                        -> IO Text
reactRenderViewToString includeStatic (View rc) = do
  let element = elementToM () $ NewViewElement rc "main" (const $ return ())
  (e, _) <- mkReactElement @'EventHandlerCode (const $ pure ()) (ReactThis nullRef) element
  sRef <- (if includeStatic then js_ReactRenderStaticMarkup else js_ReactRenderToString) e
  mtxt <- fromJSVal sRef
  maybe (error "Unable to convert string return to Text") return mtxt


-- $dispatcher
--
-- The dispatcher is the central hub that manages all data flow in a Flux application.  It has no
-- logic of its own and all it does is distribute actions to stores.  There is no special support
-- for a dispatcher in this module, since it can be easily implemented directly using Haskell
-- functions.  The event handlers registered during rendering are expected to produce a list of
-- 'SomeStoreAction'.  The dispatcher therefore consists of Haskell functions which produce these
-- lists of 'SomeStoreAction'.  Note that this list of actions is used instead of @waitFor@ to
-- sequence actions to stores: when dispatching, we wait for the 'transform' of each action to
-- complete before moving to the next action.
--
-- In the todo example application there is only a single store, so the dispatcher just passes along
-- the action to the store.  In a larger application, the dispatcher could have its own actions and
-- produce specific actions for each store.
--
-- >dispatchTodo :: TodoAction -> [SomeStoreAction]
-- >dispatchTodo a = [SomeStoreAction todoStore a]


-- $performance
--
-- React obtains high <https://facebook.github.io/react/docs/advanced-performance.html performance>
-- from two techniques: the <https://facebook.github.io/react/docs/reconciliation.html virtual
-- DOM/reconciliation> and <https://facebook.github.io/react/docs/events.html event handlers>
-- registered on the document.
--
-- __Reconciliation__
--
-- To support fast reconciliation, React uses key properties (set by 'viewWithKey') and a
-- @shouldComponentUpdate@ lifetime class method.  The React documentation on
-- <https://facebook.github.io/react/docs/advanced-performance.html performance and immutable-js>
-- talks about using persistent data structures, which is exactly what Haskell does.  In order to
-- decide whether two @state@ or @props@ values differ, we call the haskell 'Eq' instance.
--
-- The predecessor of this library, <https://bitbucket.org/wuzzeb/react-flux react-flux>, did not
-- use Haskell's 'Eq' to decide whether store, props, or state values changed, but javascript
-- pointer equality with some ad-hoc special cases.  This meant that two equal values were detected
-- to be different if their memory representations differed, which frequently happens because of
-- lazy evaluation.  This is one of the main improvements of react-hs over react-flux.  Note that
-- the new approach means that the performance of 'Eq' has now some impact on performance.  If you
-- have evidence that this needs tuning in your project, we'd like to know -- please open an issue!
--
-- __Events__
--
-- For events, React registers only global event handlers and also keeps event objects (the object
-- passed to the handlers) in a pool and re-uses them for successive events.  We want to parse this
-- event object lazily so that only properties actually accessed are parsed.
--
-- To make this possible while preventing react from recycling a foreign event object before it is
-- fully used by the pure function that uses it as input, all react events are detached from the
-- pool in 'React.Flux.on' and all of its variants.
--
-- The predecessor of this library, <https://bitbucket.org/wuzzeb/react-flux react-flux>, had an
-- 'NFData' constraint on 'SomeStoreAction', and the user was forced construct the event response
-- strictly and synchronously.  This relied on calling `seq` and `deepseq` in all the right places;
-- using a Monad (as we do it now, at least under the hood in the implementation of 'runEvent') for
-- effect sequencing is both more idiomatic and more robust.  We do not expect any more performance
-- issues from not using the event pool for heap recycling than from the overhead of strictifying
-- the event handling.  If you disagree or have evidence to the contracty, please open an issue.


-- ffi

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
