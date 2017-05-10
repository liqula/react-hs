-- | This module contains some useful combinators I have come across as I built a large
-- react-flux application.  None of these are required to use React.Flux, they just reduce somewhat
-- the typing needed to create rendering functions.
{-# LANGUAGE DeriveAnyClass #-}
module React.Flux.Combinators (
    clbutton_
  , cldiv_
  , faIcon_
  , foreign_, foreignClass, rawJsRendering
  , labeledInput_
  , style
) where

import Control.Monad.Writer (runWriter)
import Data.Monoid ((<>))
import JavaScript.Array as JSA
import React.Flux.DOM
import React.Flux.Internal
import React.Flux.PropertiesAndEvents
import GHCJS.Types (JSVal)

foreign import javascript unsafe
    "$r = window[$1] || (typeof global !== 'undefined' ? global[$1] : undefined)"
    js_lookupWindow :: JSString -> JSVal

-- | A wrapper around 'foreignClass' that looks up the class on the `window`.  I use it for several
-- third-party react components.  For example, with <https://github.com/reactjs/react-modal react-modal>,
-- assuming `window.ReactModal` contains the definition of the class,
--
-- >foreign_ "ReactModal" [ "isOpen" @= isModelOpen myProps
-- >                      , callback "onRequestClose" $ dispatch closeModel
-- >                      , "style" @= Aeson.object [ "overlay" @= Aeson.object ["left" $= "50%", "right" $= "50%"]]
-- >                      ] $ do
-- >    h1_ "Hello, World!"
-- >    p_ "...."
--
-- Here is another example using <https://github.com/JedWatson/react-select react-select>:
--
-- >reactSelect_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
-- >reactSelect_ props = foreign_ "Select" props mempty
-- >
-- >someView :: ReactView ()
-- >someView = defineView "some view" $ \() ->
-- >    reactSelect_
-- >        [ "name" $= "form-field-name"
-- >        , "value" $= "one"
-- >        , "options" @= [ object [ "value" .= "one", "label" .= "One" ]
-- >                       , object [ "value" .= "two", "label" .= "Two" ]
-- >                       ]
-- >        , callback "onChange" $ \(i :: String) -> dispatch $ ItemChangedTo i
-- >        ]
foreign_ :: JSString -- ^ this should be the name of a property on `window` which contains a react class.
         -> [PropertyOrHandler handler] -- ^ properties
         -> ReactElementM handler a -- ^ children
         -> ReactElementM handler a
foreign_ x = foreignClass (js_lookupWindow x)

-- | Create a 'ReactElement' for a class defined in javascript.  See
-- 'React.Flux.Combinators.foreign_' for a convenient wrapper and some examples.
foreignClass :: JSVal -- ^ The javascript reference to the class
             -> [PropertyOrHandler eventHandler] -- ^ properties and handlers to pass when creating an instance of this class.
             -> ReactElementM eventHandler a -- ^ The child element or elements
             -> ReactElementM eventHandler a
foreignClass name attrs (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ForeignElement (Right $ ReactViewRef name) attrs childEl

-- | Inject arbitrary javascript code into the rendering function.  This is very low level and should only
-- be used as a last resort when interacting with complex third-party react classes.  For the most part,
-- third-party react classes can be interacted with using 'foreignClass' and the various ways of creating
-- properties.
rawJsRendering :: (JSVal -> JSArray -> IO JSVal)
                  -- ^ The raw code to inject into the rendering function.  The first argument is the 'this' value
                  -- from the rendering function so points to the react class.  The second argument is the result of
                  -- rendering the children so is an array of react elements.  The return value must be a React element.
               -> ReactElementM handler () -- ^ the children
               -> ReactElementM handler ()
rawJsRendering trans (ReactElementM child) =
    let (a, childEl) = runWriter child
        trans' thisVal childLst =
          ReactElementRef <$> trans thisVal (JSA.fromList $ map reactElementRef childLst)
     in elementToM a $ RawJsElement trans' childEl


-- | A 'div_' with the given class name (multiple classes can be separated by spaces).  This is
-- useful for defining rows and columns in your CSS framework of choice.  I use
-- <http://purecss.io/forms/ Pure CSS> so I use it something like:
--
-- >cldiv_ "pure-g" $ do
-- >    cldiv_ "pure-u-1-3" $ p_ "First Third"
-- >    cldiv_ "pure-u-1-3" $ p_ "Middle Third"
-- >    cldiv_ "pure-u-1-3" $ p_ "Last Third"
cldiv_ :: JSString -> ReactElementM handler a -> ReactElementM handler a
cldiv_ cl = div_ ["className" &= cl]

-- | A 'button_' with the given class names and `onClick` handler.
--
-- >clbutton_ "pure-button button-success" (dispatch LaunchTheMissiles) $ do
-- >    faIcon_ "rocket"
-- >    "Launch the missiles!"
clbutton_ :: JSString  -- ^ class names separated by spaces
          -> handler -- ^ the onClick handler for the button
          -> ReactElementM handler a -- ^ the children
          -> ReactElementM handler a
clbutton_ cl h = button_ ["className" &= cl, onClick (\_ _ -> h)]

-- | A 'label_' and an 'input_' together.  Useful for laying out forms.  For example, a
-- stacked <http://purecss.io/forms/ Pure CSS Form> could be
--
-- >form_ ["className" $= "pure-form pure-form-stacked"] $
-- >    fieldset_ $ do
-- >        legend_ "A stacked form"
-- >        labeledInput_ "email" "Email" ["type" $= "email"]
-- >        labeledInput_ "password"
-- >            ($(message "password-label" "Your password") [])
-- >            ["type" $= "password"]
--
-- The second 'labeledInput_' shows an example using "React.Flux.Addons.Intl".
labeledInput_ :: JSString -- ^ the ID for the input element
              -> ReactElementM handler () -- ^ the label content.  This is wrapped in a 'label_' with a `htmlFor` property
                                          -- equal to the given ID.
              -> [PropertyOrHandler handler] -- ^ the properties to pass to 'input_'.  A property with key `id` is added to this list of properties.
              -> ReactElementM handler ()
labeledInput_ ident lbl props = label_ ["htmlFor" &= ident] lbl <> input_ (("id" &= ident):props)

-- | A <http://fortawesome.github.io/Font-Awesome/ Font Awesome> icon.  The given string is prefixed
-- by `fa fa-` and then used as the class for an `i` element.  This allows you to icons such as
--
-- >faIcon_ "fighter-jet" -- produces <i class="fa fa-fighter-jet">
-- >faIcon_ "refresh fa-spin" -- produces <i class="fa fa-refresh fa-spin">
faIcon_ :: JSString -> ReactElementM handler ()
faIcon_ cl = i_ ["className" &= ("fa fa-" <> cl)] mempty

-- | A simple combinator to easily write <https://facebook.github.io/react/tips/inline-styles.html inline styles>
-- on elements.
style :: [(JSString,JSString)] -> PropertyOrHandler handler
style = nestedProperty "style" . map (\(n,a) -> (n &= a))
