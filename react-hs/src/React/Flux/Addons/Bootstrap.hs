-- | Bindings to <http://react-bootstrap.github.io/ React-Bootstrap>.  To use this
-- binding, include the browser-global version of @react-bootstrap.js@ so that @window.ReactBootstrap@
-- is defined.  You can then use 'bootstrap_' inside your rendering functions.
--
-- Note: I initially wrote these bindings when I was first starting React, but after some experience
-- I think that bootstrap and react just do not fit together and I no longer use bootstrap and react
-- together.  While I am leaving this here for backwards compatibility, I suggest you use a different
-- library.  I am currently using <http://www.material-ui.com Material UI> and accessing the
-- components using @foreign_@.

module React.Flux.Addons.Bootstrap (
    bootstrap_
) where

import React.Flux

import GHCJS.Types (JSVal, JSString)
import React.Flux.Internal (toJSString)

-- | A bootstrap <http://react-bootstrap.github.io/components.html component>.  For example,
--
-- >bootstrap_ "Alert" [ "bsStyle" $= "danger"
-- >                   , callback "onDismiss" $ dispatch CloseAlert
-- >                   ] $
-- >    p_ "Hello, World!"
-- >
-- >bootstrap_ "Nav" [ "activeKey" @= (1 :: Int)
-- >                 , callback "onSelect" $ \(i :: Int) -> dispatch $ TabChange i
-- >                 ] $ do
-- >    bootstrap_ "NavItem" ["eventKey" @= (1 :: Int)] "Item 1"
-- >    bootstrap_ "NavItem" ["eventKey" @= (2 :: Int)] "Item 2"
-- >    bootstrap_ "NavItem" ["eventKey" @= (3 :: Int)] "Item 3"
bootstrap_ :: String
               -- ^ The component name.   Uses @window[\'ReactBootstrap\'][name]@ to find the class, so
               -- the name can be anything exported to the @window.ReactBoostrap@ object.
           -> [PropertyOrHandler eventHandler]
               -- ^ Properties and callbacks to pass to the ReactBootstrap class.  You can use 'callback'
               -- to create function properties.
           -> ReactElementM eventHandler a -- ^ The child or children of the component.
           -> ReactElementM eventHandler a
bootstrap_ n = foreignClass (js_ReactBootstrap $ toJSString n)

#ifdef __GHCJS__

foreign import javascript unsafe
    "window['ReactBootstrap'][$1]"
    js_ReactBootstrap :: JSString -> JSVal

#else

js_ReactBootstrap :: JSString -> JSVal
js_ReactBootstrap _ = error "js_ReactBootstrap only works with GHCJS"

#endif
