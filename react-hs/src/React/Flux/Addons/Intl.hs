-- | Bindings to the <https://github.com/yahoo/react-intl ReactIntl> library version 2, which allows easy
-- formatting of numbers, dates, times, relative times, pluralization, and translated messages. This
-- library can be used for formatting and pluralization even if you intend to present your application
-- in a single language and locale.
--
-- To use these bindings, you need to provide the @ReactIntl@ variable.  In the browser you can just
-- load the @react-intl.min.js@ script onto the page so that @window.ReactIntl@ exists.  If you are
-- running in node, execute @ReactIntl = require(\"ReactIntl\");@ so that @global.ReactIntl@
-- exists.  When compiling with closure, protect the ReactIntl variable as follows:
--
-- >(function(global, React, ReactDOM, ReactIntl) {
-- >contents of all.js
-- >})(window, window['React'], window['ReactDOM'], window['ReactIntl']);
--
-- __Using with a single locale and no translations__.  If you intend to present your application in
-- a single language, you can still use this module for formatting.  Add a call to 'intlProvider_'
-- to the top of your app with a hard-coded locale and @Nothing@ for the messages.  You can then use
-- anything in the /Formatting/ section like 'int_', 'relativeTo_', and 'message', where 'message'
-- will just always use the default message provided in the source code (helpful for templating).
-- If you want to specify the locale so dates and numbers are formatted in the user's locale, it is
-- strongly recommended to set the locale from the server based on the @Accept-Language@ header
-- and/or a user setting so that the page as a whole is consistint.  I have the server set a
-- variable on @window@ for the locale to use, and then pass that locale into 'intlProvider_'.
--
-- __Translations__.  The react-intl philosophy is that messages should be defined in the source
-- code instead of kept in a separate file.  To support translations, a tool (in this case Template
-- Haskell) is used to extract the messages from the source code into a file given to the
-- translators.  The result of the translation is then used to replace the default message given in
-- the source code.
--
--   1. Use the functions in the /Formatting/ section like 'int_', 'relativeTo_', and 'message'
--   inside your rendering functions.
--
--   2. At the bottom of each file which contains messages, add a call to 'writeIntlMessages'.  This
--   is a template haskell function which during compilation will produce a file containing all the
--   messages found within the haskell module.
--
--   3. Give these message files to your translators.  The translation results will then need to be
--   converted into javascript files in the format expected by ReactIntl, which is a javascript
--   object with keys the 'MessageId's and value the translated message.  For example, each translation
--   could result in a javascript file such as the following:
--
--       @
--       window.myMessages = window.myMessages || {};
--       window.myMessages["fr-FR"] = {
--          "num_photos": "{name} {numPhotos, plural, =0 {n'a pas pris de photographie.} =1 {a pris une photographie.} other {a pris # photographies.}",
--          ...
--       };
--       @
--
--   4. Based on the @Accept-Language@ header and/or a user setting, the server includes the
--   appropriate translation javascript file and sets a variable on window containing the locale to
--   use.  Note that no translation javascript file is needed if the default messages from the
--   source code should be used.
--
--        @
--        \<script type="text\/javascript"\>window.myIntialConfig = { "locale": "fr-FR" };\<\/script\>
--        \<script src="path\/to\/translations.fr-FR.js"\>\<\/script\>
--        @
--
--   5. Add a call to 'intlProvider_' at the top of your application, passing the locale and the
--   messages.
--
--        @
--        foreign import javascript unsafe
--          "$r = window[\'myInitialConfig\'][\'locale\']"
--          js_initialLocale :: JSString
--
--        foreign import javascript unsafe
--          "window[\'myMessages\'] ? window[\'myMessages\'][$1] : null"
--          js_myMessages :: JSString -> JSVal
--
--        myApp :: ReactView ()
--        myApp = defineView "my application" $ \() -> do
--            intlProvider_ (JSString.unpack js_initialLocale) (Just $ js_myMessages js_initialLocale) $
--              ...
--        @
--
--        If you want to allow changing the locale without a page refresh, just load the initial
--        locale into a store and use a controller-view to pass the locale and lookup the messages
--        for 'intlProvider_'.

{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

module React.Flux.Addons.Intl(
    intlProvider_

  -- * Formatting
  , IntlProperty
  , iprop

  -- ** Numbers
  , int_
  , double_
  , formattedNumber_
  , formattedNumberProp

  -- ** Dates and Times
  , DayFormat(..)
  , shortDate
  , day_
  , TimeFormat(..)
  , shortDateTime
  , utcTime_
  , formattedDate_
  , formattedDateProp
  , dayToJSVal
  , timeToJSVal
  , dayProp
  , timeProp

  -- ** Relative Times
  , relativeTo_
  , formattedRelative_
  , formattedRelativeProp

  -- ** Plural
  , plural_
  , pluralProp

  -- ** Messages
  , MessageId
  , message
  , message'
  , messageProp
  , messageProp'
  , htmlMsg
  , htmlMsg'
  , formattedMessage_
  , formattedHtmlMessage_

  -- * Translation
  , Message(..)
  , writeIntlMessages
  , intlFormatJson
  , intlFormatJsonWithoutDescription
  , intlFormatAndroidXML
) where

import Control.Monad (when, forM_)
import Data.Aeson (Object, Value(Object), object, (.=))
import Data.Char (ord, isPrint)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Time
import Language.Haskell.TH (runIO, Q, Loc, location, ExpQ)
import Language.Haskell.TH.Syntax (liftString, qGetQ, qPutQ, reportWarning, Dec)
import React.Flux
import React.Flux.Internal (PropertyOrHandler(PropertyFromContext), toJSString, IsEventHandler)
import System.IO (withFile, IOMode(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHCJS.Types (JSString, JSVal)
import GHCJS.Marshal (ToJSVal(..))
import qualified JavaScript.Object as JSO

-- | Convert a day to a javascript Date.  This is useful to pass a date as a property to a
-- 'message'.  Note that @JSVal@ is an instance of @ToJSVal@ so the result of 'dayToJSVal' can
-- be passed as a property via '(&=)'.
dayToJSVal :: Day -> JSVal
dayToJSVal day = js_mkDate (fromIntegral y) m d
    where
        (y, m, d) = toGregorian day

-- | Convert a UTCTime to a javascript date object.  This is useful to pass a time as a property
-- to a 'message'.  Note that @JSVal@ is an instance of @ToJSVal@ so the result of 'timeToJSVal' can
-- be passed as a property via '(&=)'.
timeToJSVal :: UTCTime -> JSVal
timeToJSVal (UTCTime uday time) = js_mkDateTime (fromIntegral year) month day hour minute sec milli
    where
        (year, month, day) = toGregorian uday
        TimeOfDay hour minute pSec = timeToTimeOfDay time
        (sec, fracSec) = properFraction pSec
        milli = round $ fracSec * 1000 -- milli is 10^3

-- | A property and value that is passed to the intl elements below in situations where
-- React elements can not be used.
--
-- Some of the intl elements below such as 'message' (among others) allow other React elements to
-- be passed as properties.  In this case, 'PropertyOrHandler' is used as the
-- type for the parameters and elements can be passed using 'elementProperty'.
-- But for some intl elements below such as 'messageProp', a limitation on the internals of this
-- @react-flux@ package disallow element properties to be created and so only basic javascript
-- values can be passed.  In these situations, the type 'IntlProperty' is used to restrict the
-- properties to basic javascript values.
data IntlProperty = forall ref. ToJSVal ref => IntlProperty JSString ref

-- | Create an 'IntlProperty' from a property name and anything that can be converted to a
-- javascript value. (@ToJSVal@ lives in @GHCJS.Foreign.Marshal@ module in the @ghcjs-base@ package.)
iprop :: ToJSVal v => JSString -> v -> IntlProperty
iprop = IntlProperty

-- | Convert a day to a javascript date and set it as a property.  This is primarily useful to be able
-- to pass a date as a property to 'messageProp'.
dayProp :: JSString -> Day -> IntlProperty
dayProp n d = IntlProperty n (dayToJSVal d)

-- | Convert a 'UTCTime' to a javascript date and set it as a property.  This is primarily useful to
-- be able to pass a time as a property to 'messageProp'.
timeProp :: JSString -> UTCTime -> IntlProperty
timeProp n t = IntlProperty n (timeToJSVal t)

data ContextApiCall a = ContextApiCall JSString a [IntlProperty] JSVal

instance ToJSVal a => ToJSVal (ContextApiCall a) where
    toJSVal (ContextApiCall name a props ctx) = do
        aRef <- toJSVal a
        propsObj <- JSO.create
        forM_ props $ \(IntlProperty n p) -> do
            pRef <- toJSVal p
            JSO.setProp n pRef propsObj
        js_callContextAPI ctx name aRef propsObj

formatCtx :: ToJSVal a => JSString -> JSString -> a -> [IntlProperty] -> PropertyOrHandler handler
formatCtx name func val options = PropertyFromContext name $ ContextApiCall func val options


-- | Use the IntlProvider to set the @locale@, @formats@, and @messages@ property.
intlProvider_ :: IsEventHandler eventHandler
              => JSString -- ^ the locale to use
              -> Maybe JSVal
                  -- ^ A reference to translated messages, which must be an object with keys
                  -- 'MessageId' and value the translated message.  Set this as Nothing if you are not using
                  -- translated messages, since either @Nothing@ or a null JSVal will cause the messages
                  -- from the source code to be used.
              -> Maybe Object
                  -- ^ An object to use for the @formats@ parameter which allows custom formats.  I
                  -- suggest you use custom formats only for messages.  Custom formats for numbers
                  -- and dates not in a message is better done by writing a small Haskell utility
                  -- function wrapping for example 'formattedNumber_'.
              -> ReactElementM eventHandler a -- ^ The children of this element.  All descendents will use the given locale and messages.
              -> ReactElementM eventHandler a
intlProvider_ locale mmsgs mformats = foreignClass js_intlProvider props
    where
        props = catMaybes [ Just ("locale" &= locale)
                          , (property "messages") <$> mmsgs
                          , (property "formats" . Object) <$> mformats
                          ]

--------------------------------------------------------------------------------
--- Numbers
--------------------------------------------------------------------------------

-- | Format an integer using 'formattedNumber_' and the default style.
int_ :: IsEventHandler eventHandler => Int -> ReactElementM eventHandler ()
int_ i = formattedNumber_ [ "value" @= i ]

-- | Format a double using 'formattedNumber_' and the default style.
double_ :: IsEventHandler eventHandler => Double -> ReactElementM eventHandler ()
double_ d = formattedNumber_ [ "value" @= d ]

-- | A <http://formatjs.io/react/#formatted-number FormattedNumber> which allows arbitrary properties
-- and therefore allows control over the style and format of the number.  The accepted properties are
-- any options supported by
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NumberFormat Intl.NumberFormat>.
formattedNumber_ :: IsEventHandler eventHandler => [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedNumber_ props = foreignClass js_formatNumber props mempty

-- | Format a number as a string, and then use it as the value for a property.  'int_', 'double_',
-- or 'formattedNumber_' should be prefered because as components they can avoid re-rendering when
-- the number has not changed. 'formattedNumberProp' is needed if the formatted number has to be
-- a property on another element, such as the placeholder for an input element.
formattedNumberProp :: IsEventHandler handler => ToJSVal num
                    => JSString -- ^ the property to set
                    -> num -- ^ the number to format
                    -> [IntlProperty] -- ^ any options accepted by
                                      -- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NumberFormat Intl.NumberFormat>
                    -> PropertyOrHandler handler
formattedNumberProp name x options = formatCtx name "formatNumber" x options

--------------------------------------------------------------------------------
-- Date/Time
--------------------------------------------------------------------------------

-- | How to display a date.  Each non-Nothing component will be displayed while the Nothing
-- components will be ommitted.  If everything is nothing, then it is assumed that year, month, and
-- day are each numeric.
--
-- These properties coorespond directly the options accepted by
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat Intl.DateTimeFormat>.
data DayFormat = DayFormat {
    weekdayF :: Maybe JSString -- ^ possible values are narrow, short, and long
  , eraF :: Maybe JSString -- ^ possible values are narrow, short, and long
  , yearF :: Maybe JSString -- ^ possible values are numeric and 2-digit
  , monthF :: Maybe JSString -- ^ possible values are numeric, 2-digit, narrow, short, and long
  , dayF :: Maybe JSString -- ^ possible values are numeric and 2-digit
} deriving Show

-- | Convert a format to the properties accepted by FormattedDate
dayFtoProps :: IsEventHandler handler => DayFormat -> [PropertyOrHandler handler]
dayFtoProps (DayFormat w e y m d) = catMaybes
    [ ("weekday"&=) <$> w
    , ("era"&=) <$> e
    , ("year"&=) <$> y
    , ("month"&=) <$> m
    , ("day"&=) <$> d
    ]

-- | A short day format, where month is \"short\" and year and day are \"numeric\".
shortDate :: DayFormat
shortDate = DayFormat
  { weekdayF = Nothing
  , eraF = Nothing
  , yearF = Just "numeric"
  , monthF = Just "short"
  , dayF = Just "numeric"
  }

-- | How to display a time.  Each non-Nothing component will be displayed while Nothing components
-- will be ommitted.
--
-- These properties coorespond directly the options accepted by
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat Intl.DateTimeFormat>.
data TimeFormat = TimeFormat {
    hourF :: Maybe JSString -- ^ possible values are numeric and 2-digit
  , minuteF :: Maybe JSString -- ^ possible values are numeric and 2-digit
  , secondF :: Maybe JSString -- ^ possible values are numeric and 2-digit
  , timeZoneNameF :: Maybe JSString -- ^ possible values are short and long
} deriving Show

-- | Convert a time format to properties for the FormattedDate element
timeFtoProps :: IsEventHandler handler => TimeFormat -> [PropertyOrHandler handler]
timeFtoProps (TimeFormat h m s t) = catMaybes
    [ ("hour"&=) <$> h
    , ("minute"&=) <$> m
    , ("second"&=) <$> s
    , ("timeZoneName"&=) <$> t
    ]

-- | A default date and time format, using 'shortDate' and then numeric for hour, minute, and
-- second.
shortDateTime :: (DayFormat, TimeFormat)
shortDateTime = (shortDate, TimeFormat
  { hourF = Just "numeric"
  , minuteF = Just "numeric"
  , secondF = Just "numeric"
  , timeZoneNameF = Nothing
  })

-- | Display a 'Day' in the given format using the @FormattedDate@ class and then wrap it in a
-- HTML5 <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time time> element.
day_ :: IsEventHandler eventHandler => DayFormat -> Day -> ReactElementM eventHandler ()
day_ fmt day = time_ [property "dateTime" dateRef] $ foreignClass js_formatDate props mempty
    where
        dateRef = dayToJSVal day
        props = property "value" dateRef : dayFtoProps fmt

-- | Display a 'UTCTime' using the given format.  Despite giving the time in UTC, it will be
-- displayed to the user in their current timezone.  In addition, wrap it in a HTML5
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time time> element.
utcTime_ :: IsEventHandler eventHandler => (DayFormat, TimeFormat) -> UTCTime -> ReactElementM eventHandler ()
utcTime_ (dayFmt, timeF) t = time_ [property "dateTime" timeRef] $ foreignClass js_formatDate props mempty
    where
        timeRef = timeToJSVal t
        props = property "value" timeRef : (dayFtoProps dayFmt ++ timeFtoProps timeF)

-- | A raw <http://formatjs.io/react/#formatted-date FormattedDate> class which allows custom
-- properties to be passed.  The given 'Day' or 'UTCTime' will be converted to a javascript Date
-- object and passed in the @value@ property.  The remaining properties can be any properties that
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat Intl.DateTimeFormat>
-- accepts.  For example, you could pass in \"timeZone\" to specify a specific timezone to display.
formattedDate_ :: IsEventHandler eventHandler
               => Either Day UTCTime -> [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedDate_ t props = foreignClass js_formatDate (valProp:props) mempty
    where
        valProp = property "value" $ either dayToJSVal timeToJSVal t

-- | Format a day or time as a string, and then use it as the value for a property.  'day_',
-- 'utcTime_', or 'formattedDate_' should be prefered because as components they can avoid re-rendering when
-- the date has not changed. 'formattedDateProp' is needed if the formatted date has to be
-- a property on another element, such as the placeholder for an input element.
formattedDateProp :: IsEventHandler eventHandler
                  => JSString -- ^ the property to set
                  -> Either Day UTCTime -- ^ the day or time to format
                  -> [IntlProperty] -- ^ Any options supported by
                                    -- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat
                                    -- Intl.DateTimeFormat>.
                  -> PropertyOrHandler eventHandler
formattedDateProp name (Left day)   = formatCtx name "formatDate" (dayToJSVal day)
formattedDateProp name (Right time) = formatCtx name "formatTime" (timeToJSVal time)

-- | Display the 'UTCTime' as a relative time.  In addition, wrap the display in a HTML5
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time time> element.
relativeTo_ :: IsEventHandler eventHandler => UTCTime -> ReactElementM eventHandler ()
relativeTo_ t = time_ [property "dateTime" timeRef] $ foreignClass js_formatRelative [property "value" timeRef] mempty
    where
        timeRef = timeToJSVal t

-- | Format the given UTCTime using the <http://formatjs.io/react/#formatted-relative FormattedRelative>
-- class to display a relative time to now.  The given 'UTCTime' is passed in the value property.
-- The supported style/formatting properties are \"units\" which can be one of second, minute, hour,
-- day, month, or year and \"style\" which if given must be numeric.
formattedRelative_ :: IsEventHandler eventHandler => UTCTime -> [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedRelative_ t props = foreignClass js_formatRelative (property "value" (timeToJSVal t) : props) mempty

-- | Format a time as a relative time string, and then use it as the value for a property.
-- 'relativeTo_' or 'formattedRelative_' should be prefered because as components they can avoid re-rendering when
-- the date has not changed. 'formattedRelativeProp' is needed if the formatted date has to be
-- a property on another element, such as the placeholder for an input element.
formattedRelativeProp :: IsEventHandler eventHandler
                      => JSString -- ^ te property to set
                      -> UTCTime -- ^ the time to format
                      -> [IntlProperty] -- ^ an object with properties \"units\" and \"style\".  \"units\" accepts values second, minute, hour
                                        -- day, month, or year and \"style\" accepts only the value \"numeric\".
                      -> PropertyOrHandler eventHandler
formattedRelativeProp name time options = formatCtx name "formatRelative" (timeToJSVal time) options

--------------------------------------------------------------------------------
-- Plural
--------------------------------------------------------------------------------

-- | A simple plural formatter useful if you do not want the full machinery of messages.  This does
-- not support translation, for that you must use messages which via the ICU message syntax support
-- pluralization.  The properties passed to 'plural_' must be @value@, and then at least one of the
-- properties from @other@, @zero@, @one@, @two@, @few@, @many@.
plural_ :: IsEventHandler eventHandler => [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
plural_ props = foreignClass js_formatPlural props mempty

-- | Format a number properly based on pluralization, and then use it as the value for a property.
-- 'plural_' should be preferred, but 'pluralProp' can be used in places where a component is not
-- possible such as the placeholder of an input element.
pluralProp :: (IsEventHandler eventHandler, ToJSVal val) => JSString -> val -> [IntlProperty] -> PropertyOrHandler eventHandler
pluralProp name val options = formatCtx name "formatPlural" val options

--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------

-- | An identifier for a message, must be globally unique.
type MessageId = T.Text

-- | A message.
data Message = Message {
    msgDescription :: T.Text -- ^ A description intended to provide context for translators.
  , msgDefaultMsg :: T.Text -- ^ The default message written in <http://formatjs.io/guides/message-syntax/ ICU message syntax>.
} deriving Show

-- | This is the type stored in the Q monad with qGetQ and qPutQ
type MessageMap = H.HashMap MessageId (Message, Loc)

-- | Utility function to build the properties for FormattedMessage.
messageToProps :: IsEventHandler eventHandler
               => MessageId -> Message -> [PropertyOrHandler eventHandler] -> [PropertyOrHandler eventHandler]
messageToProps i (Message desc m) props = ["id" @= i, "description" @= desc, "defaultMessage" @= m, nestedProperty "values" props]

-- | Render a message and also record it during compilation.  This template haskell
-- splice produces an expression of type @[PropertyOrHandler eventHandler] -> ReactElementM eventHandler
-- ()@, which should be passed the values for the message.  For example,
--
-- >li_ ["id" $= "some-id"] $
-- >    $(message "num_photos" "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}} {takenAgo}.")
-- >        [ "name" $= "Neil Armstrong"
-- >        , "numPhotos" @= (100 :: Int)
-- >        , elementProperty "takenAgo" $ relativeTo_ (UTCTime (fromGregorian 1969 7 20) (2*60*60 + 56*60))
-- >        ]
--
-- This will first lookup the 'MessageId' (in this case @num_photos@) in the  @messages@ parameter passed to 'intlProvider_'.
-- If no messages were passed, 'intlProvider_' was not called, or the 'MessageId' was not found, the default message is used.
--
-- In my project, I create a wrapper around 'message' which sets the 'MessageId' as the sha1 hash of
-- the message.  I did not implement it in react-flux because I did not want to add cryptohash as a
-- dependency.  For example,
--
-- >import Crypto.Hash (hash, SHA1)
-- >import qualified Data.Text as T
-- >import qualified Data.Text.Encoding as T
-- >
-- >msg :: T.Text -> ExpQ
-- >msg txt = message (T.pack $ show (hash (T.encodeUtf8 txt) :: Digest SHA1)) txt
message :: MessageId
        -> T.Text -- ^ The default message written in <http://formatjs.io/guides/message-syntax/ ICU message syntax>.
                  -- This message is used if no translation is found, and is also the message given to the translators.
        -> ExpQ --Q (TExp ([PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()))
message ident m = formatMessage [|js_formatMsg|] ident $ Message "" m

-- | Similar to 'message', but produce an expression of type @['IntlProperty'] -> PropertyOrHandler handler@,
-- which should be passed the values for the message.  This allows you to format messages in places
-- where using a component like 'message' is not possible, such as the placeholder of input
-- elements. 'message' should be prefered since it can avoid re-rendering the formatting if the
-- value has not changed.
--
-- >import Data.Aeson ((.=))
-- >
-- >input_ [ "type" $= "numeric"
-- >       , $(messageProp "placeholder" "ageplaceholder" "Hello {name}, enter your age")
-- >             [ "name" .= nameFrom storeData ]
-- >       ]
messageProp :: T.Text -- ^ the property name to set
            -> MessageId -- ^ the message identifier
            -> T.Text -- ^ the default message written in ICU message syntax.
            -> ExpQ
messageProp name ident m =
    formatMessageProp "formatMessage" name ident $ Message "" m

-- | A variant of 'message' which allows you to specify some context for translators.
message' :: MessageId
         -> T.Text -- ^ A description indented to provide context for translators
         -> T.Text -- ^ The default message written in ICU message syntax
         -> ExpQ --Q (TExp ([PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()))
message' ident descr m = formatMessage [|js_formatMsg|] ident $ Message descr m

-- | A varient of 'messageProp' which allows you to specify some context for translators.
messageProp' :: T.Text -- ^ property to set
             -> MessageId
             -> T.Text -- ^ A description intended to provide context for translators
             -> T.Text -- ^ The default message written in ICU message syntax
             -> ExpQ
messageProp' name ident descr m =
    formatMessageProp "formatMessage" name ident $ Message descr m

-- | Similar to 'message' but use a @FormattedHTMLMessage@ which allows HTML inside the message.  It
-- is recomended that you instead use 'message' together with 'elementProperty' to include rich text
-- inside the message.  This splice produces a value of type @[PropertyOrHandler
-- eventHandler] -> ReactElementM eventHandler ()@, the same as 'message'.
htmlMsg :: MessageId
        -> T.Text -- ^ default message written in ICU message syntax
        -> ExpQ
htmlMsg ident m = formatMessage [|js_formatHtmlMsg|] ident $ Message "" m

-- | A variant of 'htmlMsg' that allows you to specify some context for translators.
htmlMsg' :: MessageId
         -> T.Text -- ^ A description intended to provide context for translators
         -> T.Text -- ^ The default message written in ICU message syntax
         -> ExpQ
htmlMsg' ident descr m = formatMessage [|js_formatHtmlMsg|] ident $ Message descr m

recordMessage :: MessageId -> Message -> Q ()
recordMessage ident m = do
    curLoc <- location
    mmap :: MessageMap <- fromMaybe H.empty <$> qGetQ
    case H.lookup ident mmap of
        Just (prevMsg, prevLoc) | msgDefaultMsg m /= msgDefaultMsg prevMsg -> do
            reportWarning $ unlines
                [ "Message with id " ++ (T.unpack ident) ++ " appears twice with different messages"
                , show curLoc ++ ": " ++ (T.unpack $ msgDefaultMsg m)
                , show prevLoc ++ ": " ++ (T.unpack $ msgDefaultMsg prevMsg)
                ]
        _ -> return ()
    qPutQ $ H.insert ident (m, curLoc) mmap

-- | Utility function for messages
formatMessage :: ExpQ -> MessageId -> Message -> ExpQ --Q (TExp ([PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()))
formatMessage cls ident m = do
    recordMessage ident m
    let liftText x = [| T.pack $(liftString $ T.unpack x)|]
        liftedMsg = [| Message $(liftText $ msgDescription m) $(liftText $ msgDefaultMsg m) |]
    [|\vals -> foreignClass $cls (messageToProps $(liftText ident) $liftedMsg vals) mempty |]

formatMessageProp :: T.Text -> T.Text -> MessageId -> Message -> ExpQ -- Q (TExp ([IntlProperty] -> PropertyOrHandler eventHandler))
formatMessageProp func name ident m = do
    recordMessage ident m
    let liftedMsg = [| object ["id" .= T.pack $(liftString $ T.unpack ident), "defaultMessage" .= T.pack $(liftString $ T.unpack $ msgDefaultMsg m) ] |]
    [|\options -> formatCtx (toJSString $(liftString $ T.unpack name)) (toJSString $(liftString $ T.unpack func)) $liftedMsg options |]

-- | A raw @FormattedMessage@ element.  The given properties are passed directly with no handling.
-- Any message is not recorded in Template Haskell and will not appear in any resulting message file
-- created by 'writeIntlMessages'.
formattedMessage_ :: IsEventHandler eventHandler => [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedMessage_ props = foreignClass js_formatMsg props mempty

-- | A raw @FormattedHTMLMessage@ element.  The given properties are passed directly with no handling.
-- Any message is not recorded in Template Haskell and will not appear in any resulting message file
-- created by 'writeIntlMessages'.
formattedHtmlMessage_ :: IsEventHandler eventHandler => [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedHtmlMessage_ props = foreignClass js_formatHtmlMsg props mempty

-- | Perform an arbitrary IO action on the accumulated messages at compile time, which usually
-- should be to write the messages to a file.  Despite producing a value of type @Q [Dec]@,
-- no declarations are produced.  Instead, this is purly to allow IO to happen.  A call to this
-- function should be placed at the bottom of the file, since it only will output messages that
-- appear above the call.  Also, to provide consistency, I suggest you create a utility wrapper
-- around this function.  For example,
--
-- >{-# LANGUAGE TemplateHaskell #-}
-- >module MessageUtil where
-- >
-- >import Language.Haskell.TH
-- >import Language.Haskell.TH.Syntax
-- >import React.Flux.Addons.Intl
-- >
-- >writeMessages :: String -> Q [Dec]
-- >writeMessages name = writeIntlMessages (intlFormatJson $ "some/diretory/" ++ name ++ ".json")
--
-- Note that all paths in template haskell are relative to the directory containing the @.cabal@
-- file.  You can then use this as follows:
--
-- >{-# LANGUAGE TemplateHaskell #-}
-- >module SomeViews where
-- >
-- >import React.Flux
-- >import React.Flux.Addons.Intl
-- >import MessageUtil
-- >
-- >someView :: ReactView ()
-- >someView = defineView .... use $(message) in render ...
-- >
-- >anotherView :: ReactView ()
-- >anotherView = defineView ... use $(message) in render ...
-- >
-- >writeMessages "some-views"
writeIntlMessages :: (H.HashMap MessageId Message -> IO ()) -> Q [Dec]
writeIntlMessages f = do
    mmap :: MessageMap <- fromMaybe H.empty <$> qGetQ
    runIO $ f $ fmap fst mmap
    return []

-- | Format messages as json.  The format is an object where keys are the 'MessageId's, and the
-- value is an object with two properties, @message@ and optionally @description@.  This happens to
-- the the same format as <https://developer.chrome.com/extensions/i18n-messages chrome>, although
-- the syntax of placeholders uses ICU message syntax instead of chrome's syntax.  This does not
-- pretty-print the JSON, but I suggest before adding these messages in source control you pretty
-- print and sort by MessageIds so diffs are easy to read.  This can be done with the
-- @aeson-pretty@ package, but I did not want to add it as a dependency.
intlFormatJson :: FilePath -> H.HashMap MessageId Message -> IO ()
intlFormatJson fp mmap = BL.writeFile fp $ Aeson.encode $ Aeson.Object $ fmap f mmap
    where
        f (Message "" m) = Aeson.object [(Aeson..=) "message" m]
        f (Message desc m) = Aeson.object [(Aeson..=) "message" m, (Aeson..=) "description" desc]

-- | Format messages as json, ignoring the description.  The format is an object where the keys are
-- the 'MessageId's and the value is the message string.  This format is used by many javascript
-- libraries, so many translation tools exist.
intlFormatJsonWithoutDescription :: FilePath -> H.HashMap MessageId Message -> IO ()
intlFormatJsonWithoutDescription fp mmap = BL.writeFile fp $ Aeson.encode $ Aeson.Object $ fmap f mmap
    where
        f (Message _ m) = Aeson.String m

-- | Format messages in <http://developer.android.com/guide/topics/resources/string-resource.html Android XML>
-- format, but just using strings.  String arrays and plurals are handled in the ICU message,
-- instead of in the XML.  There are many utilities to translate these XML messages, and the format has the
-- advantage that it can include the descriptions as well as the messages.  Also, the messages are
-- sorted by 'MessageId' so that if the output is placed in source control the diffs are easy to
-- review.
intlFormatAndroidXML :: FilePath -> H.HashMap MessageId Message -> IO ()
intlFormatAndroidXML fp mmap = withFile fp WriteMode $ \handle -> do
    B.hPut handle "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    B.hPut handle "<resources>\n"
    let putText = B.hPut handle . T.encodeUtf8

    let msgs = sortBy (comparing fst) $ H.toList mmap
    forM_ msgs $ \(ident, m) -> do
        when (msgDescription m /= "") $
            putText $ "<!-- " <> escapeForXml (msgDescription m) <> " -->\n"
        -- TODO: escape!!
        putText $ "<string name=\"" <> escapeForXml ident <> "\">" <> escapeForXml (msgDefaultMsg m) <> "</string>\n"
    B.hPut handle "</resources>\n"

escapeForXml :: T.Text -> T.Text
escapeForXml = T.concatMap f
    where
        f '<' = "&lt;"
        f '>' = "&gt;"
        f '&' = "&amp;"
        f '"' = "&quot;"
        f '\'' = "&apos;"
        f x | isPrint x || x == '\n' = T.singleton x
        f x = "&#" <> T.pack (show $ ord x) <> ";"

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = ReactIntl['IntlProvider']"
    js_intlProvider :: JSVal

foreign import javascript unsafe
    "$r = ReactIntl['FormattedNumber']"
    js_formatNumber :: JSVal

foreign import javascript unsafe
    "$r = ReactIntl['FormattedDate']"
    js_formatDate :: JSVal

foreign import javascript unsafe
    "$r = ReactIntl['FormattedRelative']"
    js_formatRelative :: JSVal

foreign import javascript unsafe
    "$r = ReactIntl['FormattedPlural']"
    js_formatPlural :: JSVal

foreign import javascript unsafe
    "$r = ReactIntl['FormattedMessage']"
    js_formatMsg :: JSVal

foreign import javascript unsafe
    "$r = ReactIntl['FormattedHTMLMessage']"
    js_formatHtmlMsg :: JSVal

foreign import javascript unsafe
    "$r = (new Date($1, $2-1, $3))"
    js_mkDate :: Int -> Int -> Int -> JSVal

foreign import javascript unsafe
    "$r = (new Date(Date.UTC($1, $2-1, $3, $4, $5, $6, $7)))"
    js_mkDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> JSVal

foreign import javascript unsafe
    "$1['intl'][$2]($3, $4)"
    js_callContextAPI :: JSVal -> JSString -> JSVal -> JSO.Object -> IO JSVal

#else

js_intlProvider :: JSVal
js_intlProvider = error "js_intlProvider only works with GHCJS"

js_formatNumber :: JSVal
js_formatNumber = error "js_formatNumber only works with GHCJS"

js_formatDate :: JSVal
js_formatDate = error "js_formatDate only works with GHCJS"

js_formatRelative :: JSVal
js_formatRelative = error "js_formatRelative only works with GHCJS"

js_formatPlural :: JSVal
js_formatPlural = error "js_formatPlural only works with GHCJS"

js_formatMsg :: JSVal
js_formatMsg = error "js_formatMsg only works with GHCJS"

js_formatHtmlMsg :: JSVal
js_formatHtmlMsg = error "js_formatHtmlMsg only works with GHCJS"

js_mkDate :: Int -> Int -> Int -> JSVal
js_mkDate _ _ _ = error "js_mkDate only works with GHCJS"

js_mkDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> JSVal
js_mkDateTime _ _ _ _ _ _ _ = error "js_mkDateTime only works with GHCJS"

js_callContextAPI :: JSVal -> JSString -> JSVal -> JSO.Object -> IO JSVal
js_callContextAPI _ _ _ _ = error "js_callContextAPI only works with GHCJS"

#endif
