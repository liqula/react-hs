{-# LANGUAGE CPP, OverloadedStrings, TypeFamilies, ScopedTypeVariables, DeriveAnyClass,
             FlexibleInstances, DeriveGeneric, BangPatterns, TemplateHaskell, DataKinds, TypeApplications, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Control.DeepSeq (NFData)
import Control.Monad
import Data.Monoid ((<>))
import Data.Typeable (Typeable, Proxy(..))
import Debug.Trace
import GHC.Generics (Generic)
import Data.Time (UTCTime(..), fromGregorian)
import React.Flux
import React.Flux.Addons.Intl
import qualified Data.Text as T

import GHCJS.Types (JSVal, JSString)
import JavaScript.Array (JSArray)
import qualified Data.JSString.Text as JSS

data OutputStoreData = OutputStoreData
    deriving (Eq, Show, Typeable)

instance StoreData OutputStoreData where
    type StoreAction OutputStoreData = [T.Text]
    -- log both to the console and to js_output
    transform ss OutputStoreData = do
        mapM_ (js_output . JSS.textToJSString) ss
        trace (unlines $ map T.unpack ss) $ return OutputStoreData

initOutputStore :: IO ()
initOutputStore = registerInitialStore OutputStoreData

output :: [T.Text] -> [SomeStoreAction]
output s = [someStoreAction @OutputStoreData s]

--------------------------------------------------------------------------------
--- Events
--------------------------------------------------------------------------------

logM :: (T.Text -> Bool) -> T.Text
logM f = "alt modifier: " <> (T.pack $ show (f "Alt"))

logT :: EventTarget -> T.Text
logT t = eventTargetProp t "id"

tshow :: Show a => a -> T.Text
tshow = T.pack . show

rawShowView :: View '[Int]
rawShowView = mkView "raw show view" elemShow

eventsView :: View '[]
eventsView = mkView "events" $
    div_ $ do
        p_ ["key" $= "text"] $
             input_ [ "type" $= "text"
                    , "id" $= "keyinput"
                    , "placeholder" $= "onKeyDown"
                    , onKeyDown $ \e k -> simpleHandler $ output
                        [ "keydown"
                        , tshow e
                        , tshow k
                        , logM (keyGetModifierState k)
                        , logT (evtTarget e)
                        , logT (evtCurrentTarget e)
                        ]
                    , onFocus $ \e _ -> simpleHandler $ output
                        [ "focus"
                        , tshow e
                        --, logT $ focusRelatedTarget f
                        ]
                    ]

        p_ ["key" $= "click"] $
             label_ [ "id" $= "clickinput"
                    , onClick $ \e m -> simpleHandler $ output
                        [ "click"
                        , tshow e
                        , tshow m
                        , logM (mouseGetModifierState m)
                        --, logT (mouseRelatedTarget m)
                        ]
                    ]
           "onClick"

        p_ ["key" $= "touch"] $
             label_ [ "id" $= "touchinput"
                    , onTouchStart $ \e t -> simpleHandler $ output
                        [ "touchstart"
                        , tshow e
                        , tshow t
                        , logM (touchGetModifierState t)
                        , logT (touchTarget $ head $ touchTargets t)
                        , "endtouch"
                        ]
                    ]
           "onTouchStart"

        p_ ["key" $= "prevent"] $
             a_ [ "id" $= "some-link"
                , "href" $= "http://www.haskell.org"
                , onClick $ \_ _ -> preventDefault $ output ["Click some-link"]
                ]
                "Testing preventDefault"

        div_ ["key" $= "prop"] $
            div_ [ "id" $= "outer-div"
                 , onClick $ \_ _ -> simpleHandler $ output ["Click on outer div"]
                 , capturePhase $ onDoubleClick $ \_ _ -> stopPropagation $ output ["Double click outer div"]
                 ] $ do

                span_ [ "id" $= "inner-span"
                      , onClick $ \e _ -> stopPropagation e `seq` simpleHandler (output ["Click inner span"])
                      , onDoubleClick $ \_ _ -> simpleHandler $ output ["Double click inner span"]
                      ]
                      "Testing stopPropagation"

        p_ [ "id" $= "raw-show-view", "key" $= "raw"] $ view_ rawShowView "raw" 42

--------------------------------------------------------------------------------
--- Stores and should component update
--------------------------------------------------------------------------------

instance UnoverlapAllEq String
instance UnoverlapAllEq Int

data Character = Character !Int !String
    deriving (Typeable, Eq)

instance UnoverlapAllEq Character

instance Show Character where
  show (Character i s) = "C" ++ show i ++ " - " ++ s

data CharacterPair = CharacterPair {
    c1 :: !Character
  , c2 :: !Character
} deriving (Typeable, Eq)

instance UnoverlapAllEq CharacterPair

instance Show CharacterPair where
  show (CharacterPair x1 x2) = show x1 ++ ", " ++ show x2

data Humans = Humans
  { h1 :: !CharacterPair
  , h2 :: !CharacterPair
  } deriving (Typeable, Eq, Show)

instance UnoverlapAllEq Humans

instance HasField "h1" Humans CharacterPair where
  getField = h1
instance HasField "h2" Humans CharacterPair where
  getField = h2

data Tiste = Tiste
  { t1 :: !CharacterPair
  , t2 :: !CharacterPair
  } deriving (Typeable, Eq, Show)

data CharacterIndex = P1_C1 | P1_C2 | P2_C1 | P2_C2
    deriving (Show, Eq, Typeable, Generic, NFData, Bounded, Enum)

data TestStoreAction = IncrementCharacter CharacterIndex
                     | NoChangeToCharacters
    deriving (Show, Typeable, Generic, NFData)

incrChar :: Character -> Character
incrChar (Character i s) = Character (i+1) s

instance StoreData Humans where
    type StoreAction Humans = TestStoreAction
    transform NoChangeToCharacters cg = return cg
    -- normally would use lenses to update part of the store
    transform (IncrementCharacter P1_C1) cg = return $ cg { h1 = (h1 cg) { c1 = incrChar (c1 $ h1 cg) }}
    transform (IncrementCharacter P1_C2) cg = return $ cg { h1 = (h1 cg) { c2 = incrChar (c2 $ h1 cg) }}
    transform (IncrementCharacter P2_C1) cg = return $ cg { h2 = (h2 cg) { c1 = incrChar (c1 $ h2 cg) }}
    transform (IncrementCharacter P2_C2) cg = return $ cg { h2 = (h2 cg) { c2 = incrChar (c2 $ h2 cg) }}

instance StoreData Tiste where
    type StoreAction Tiste = TestStoreAction
    transform NoChangeToCharacters cg = return cg
    -- normally would use lenses to update part of the store
    transform (IncrementCharacter P1_C1) cg = return $ cg { t1 = (t1 cg) { c1 = incrChar (c1 $ t1 cg) }}
    transform (IncrementCharacter P1_C2) cg = return $ cg { t1 = (t1 cg) { c2 = incrChar (c2 $ t1 cg) }}
    transform (IncrementCharacter P2_C1) cg = return $ cg { t2 = (t2 cg) { c1 = incrChar (c1 $ t2 cg) }}
    transform (IncrementCharacter P2_C2) cg = return $ cg { t2 = (t2 cg) { c2 = incrChar (c2 $ t2 cg) }}

initCharacterStore :: IO ()
initCharacterStore = do
  registerInitialStore $
    Humans
      { h1 = CharacterPair
        { c1 = Character 10 "Quick Ben"
        , c2 = Character 20 "Whiskeyjack"
        }
      , h2 = CharacterPair
        { c1 = Character 30 "Fiddler"
        , c2 = Character 40 "Kruppe"
        }
      }
  registerInitialStore $
    Tiste
      { t1 = CharacterPair
        { c1 = Character 100 "Andarist"
        , c2 = Character 110 "Osseric"
        }
      , t2 = CharacterPair
        { c1 = Character 120 "Anomander Rake"
        , c2 = Character 130 "Korlot"
        }
      }

logWhenUpdated_ :: String -> ReactElementM handler ()
logWhenUpdated_ m = foreign_ "hsreact$log_when_updated" ["key" $= "log", "message" &= m] mempty

singleCharacterView :: View '[Character]
singleCharacterView = mkView "single-character" $ \c ->
  logWhenUpdated_ $ "Single character " ++ show c

twoCharacterView :: View '[Character, Character]
twoCharacterView = mkView "two-character" $ \ch1 ch2 ->
  logWhenUpdated_ $ "Two characters " ++ show ch1 ++ " and " ++ show ch2

pairCharacterView :: View '[CharacterPair]
pairCharacterView = mkView "pair-characters" $ \p ->
  logWhenUpdated_ $ "Pair of characters " ++ show p

statefulCharacterView :: View '[Character]
statefulCharacterView = mkStatefulView "stateful-char" (-100 :: Int) $ \s c ->
  p_ $ do
    logWhenUpdated_ ("Stateful character " ++ show c)
    span_ ["className" $= "state", "key" $= "cur state"] $ elemShow s
    button_ [ "className" $= "incr-state"
            , onClick $ \_ _ -> simpleHandler $ \s' -> ([], Just $ s' + 1)
            , "key" $= "btn"
            ]
      "Incr"

fullHumanView :: View '[Character, Character]
fullHumanView = mkControllerView @'[StoreArg Humans] "full humans" $ \humans extra1 extra2 ->
  ul_ ["id" $= "full-humans-view"] $ do
    li_ ["key" $= "header"] $ logWhenUpdated_ "All the humans, plus Andarist and Rake"
    li_ ["key" $= "11"] $ view_ singleCharacterView "11" (c1 $ h1 humans)
    li_ ["key" $= "12"] $ view_ singleCharacterView "12" (c2 $ h1 humans)
    li_ ["key" $= "21"] $ view_ singleCharacterView "21" (c1 $ h2 humans)
    li_ ["key" $= "22"] $ view_ singleCharacterView "22" (c2 $ h2 humans)
    li_ ["key" $= "112"] $ view_ twoCharacterView "112" (c1 $ h1 humans) (c2 $ h1 humans)
    li_ ["key" $= "212"] $ view_ pairCharacterView "212" (h2 $ humans)
    li_ ["key" $= "extra1"] $ view_ singleCharacterView "extra1" extra1
    li_ ["key" $= "extra2"] $ view_ singleCharacterView "extra2" extra2

tisteAndHumansView :: View '[]
tisteAndHumansView = mkControllerView @'[StoreArg Tiste] "tiste-and-humans" $ \tiste ->
  div_ ["id" $= "tiste-view"] $ do
    ul_ ["id" $= "tiste-sub-view", "key" $= "tiste-sub-view"] $ do
      li_ ["key" $= "header"] $ logWhenUpdated_ "All the tiste"
      li_ ["key" $= "11"] $ view_ singleCharacterView "11" (c1 $ t1 tiste)
      li_ ["key" $= "12"] $ view_ singleCharacterView "12" (c2 $ t1 tiste)
      li_ ["key" $= "21"] $ view_ singleCharacterView "21" (c1 $ t2 tiste)
      li_ ["key" $= "22"] $ view_ singleCharacterView "22" (c2 $ t2 tiste)
    view_ fullHumanView "humans" (c1 $ t1 tiste) (c1 $ t2 tiste)

dualCharacterView :: View '[]
dualCharacterView = mkControllerView @'[StoreArg Humans, StoreArg Tiste] "dual-characters" $ \humans tiste ->
  ul_ ["id" $= "dual-character-view"] $ do
    li_ ["key" $= "header"] $ logWhenUpdated_ "Quick Ben and Andarist"
    li_ ["key" $= "human11"] $ view_ singleCharacterView "11" (c1 $ h1 humans)
    li_ ["key" $= "tiste11"] $ view_ singleCharacterView "11" (c1 $ t1 tiste)
    li_ ["key" $= "state"] $ view_ statefulCharacterView "state" (c1 $ t2 tiste)

tisteAndSomeHumansView :: View '[]
tisteAndSomeHumansView = mkControllerView @'[StoreArg Tiste, StoreField Humans "h1" CharacterPair] "tiste-and-some-humans" $ \tiste humanPair ->
  ul_ ["id" $= "tiste-and-some-humans"] $ do
    li_ ["key" $= "header"] $ logWhenUpdated_ "Just Rake, Korlot, Quick Ben, and Whiskeyjack"
    li_ ["key" $= "t21"] $ view_ singleCharacterView "21" (c1 $ t2 tiste)
    li_ ["key" $= "t22"] $ view_ singleCharacterView "22" (c2 $ t2 tiste)
    li_ ["key" $= "h11"] $ view_ singleCharacterView "11" (c1 humanPair)
    li_ ["key" $= "h12"] $ view_ singleCharacterView "12" (c2 humanPair)

buttons_ :: forall s. (StoreData s, TestStoreAction ~ StoreAction s) => Proxy s -> T.Text -> ReactElementM 'EHView ()
buttons_ _ lbl =
  ul_ ["id" &= lbl, "key" &= lbl] $ do
    li_ ["key" $= "none"] $
      button_
        [ "id" &= (lbl <> "-none")
        , onClick $ \_ _ -> simpleHandler [someStoreAction @s NoChangeToCharacters]
        ]
        (elemText $ lbl <> " No Change")
    forM_ [minBound..maxBound] $ \idx ->
      li_ ["key" &= (lbl <> "-change-" <> tshow idx)] $
        button_
          [ "id" &= (lbl <> "-" <> tshow idx)
          , onClick $ \_ _ -> simpleHandler [someStoreAction @s $ IncrementCharacter idx]
          ] (elemText $ lbl <> tshow idx)

storeSpec :: View '[]
storeSpec = mkView "store spec" $
  div_ ["id" $= "store-spec"] $ do
    buttons_ (Proxy :: Proxy Humans) "Humans"
    buttons_ (Proxy :: Proxy Tiste) "Tiste"
    view_ tisteAndHumansView "tiste-and-human"
    view_ dualCharacterView "dual"
    view_ tisteAndSomeHumansView "tiste-and-some"

--------------------------------------------------------------------------------
--- Callback returning view
--------------------------------------------------------------------------------

callbackViewTest :: View '[Int, String]
callbackViewTest = mkView "callback view props test" $ \i s ->
    p_ [ "id" $= "callback-view-props-test"] $
        elemString $ "Props are " ++ show i ++ " and " ++ s

callbackViewWrapper :: View '[]
callbackViewWrapper = mkView "callback view wrapper" $
    div_ ["id" $= "callback-view-wrapper"] $
        foreign_ "hsreact$callback_wrapper" [ callbackRenderingView "foo" callbackViewTest ] mempty

--------------------------------------------------------------------------------
--- Intl
--------------------------------------------------------------------------------

intlSpec :: View '[]
intlSpec = mkView "intl" $
    intlProvider_ "en" (Just js_translations) Nothing $
        view_ intlSpecBody "intl-body"

intlSpecBody :: View '[]
intlSpecBody = mkView "intl body" $ div_ ["id" $= "intl-spec"] $
    ul_ $ do
        li_ ["id" $= "f-number", "key" $= "f-number"] $
            formattedNumber_ [ "value" @= (0.9 :: Double), "style" $= "percent" ]
        li_ ["id" $= "f-int", "key" $= "f-int"] $ int_ 100000
        li_ ["id" $= "f-double", "key" $= "f-double"] $ double_ 40000.2
        li_ ["id" $= "f-number-prop", "key" $= "f-number-prop"] $
            input_ [formattedNumberProp "placeholder" (123456 :: Int) []]

        let moon = fromGregorian 1969 7 20
            fullDayF = DayFormat { weekdayF = Just "long", eraF = Just "short", yearF = Just "2-digit", monthF = Just "long", dayF = Just "2-digit" }

        li_ ["id" $= "f-shortday", "key" $= "f-shortday"] $ day_ shortDate moon
        li_ ["id" $= "f-fullday", "key" $= "f-fullday"] $ day_ fullDayF moon
        li_ ["id" $= "f-date", "key" $= "f-date"] $ formattedDate_ (Left moon)
                [ "weekday" $= "short", "month" $= "short", "day" $= "numeric", "year" $= "2-digit" ]
        li_ ["id" $= "f-date-prop", "key" $= "f-date-prop"] $
            input_ [formattedDateProp "placeholder" (Left moon) []]

        let step = UTCTime moon (2*60*60 + 56*60) -- 1969-7-20 02:56 UTC
            fullT = ( fullDayF
                    , TimeFormat { hourF = Just "numeric", minuteF = Just "2-digit", secondF = Just "numeric", timeZoneNameF = Just "long" }
                    )

        li_ ["id" $= "f-shorttime", "key" $= "f-shorttime"] $ utcTime_ shortDateTime step
        li_ ["id" $= "f-fulltime", "key" $= "f-fulltime"] $ utcTime_ fullT step
        li_ ["id" $= "f-time", "key" $= "f-time"] $ formattedDate_ (Right step)
                [ "year" $= "2-digit", "month" $= "short", "day" $= "numeric"
                , "hour" $= "numeric", "minute" $= "2-digit", "second" $= "numeric"
                , "timeZoneName" $= "short"
                , "timeZone" $= "Pacific/Tahiti"
                ]
        li_ ["id" $= "f-time-prop", "key" $= "f-time-prop"] $
            input_ [formattedDateProp "placeholder" (Right step)
                    [ "year" `iprop` ("2-digit" :: String)
                    , "month" `iprop` ("short" :: String)
                    , "day" `iprop` ("2-digit" :: String)
                    , "hour" `iprop` ("numeric" :: String)
                    , "timeZone" `iprop` ("Pacific/Tahiti" :: String)
                    ]
                   ]

        li_ ["id" $= "f-relative", "key" $= "f-relative"] $ relativeTo_ step
        li_ ["id" $= "f-relative-days", "key" $= "f-relative-days"] $ formattedRelative_ step [ "units" $= "day" ]

        li_ ["id" $= "f-plural", "key" $= "f-plural"] $ plural_ [ "value" @= (100 :: Int), "one" $= "plural one", "other" $= "plural other"]
        li_ ["id" $= "f-plural-prop", "key" $= "f-plural-prop"] $
            input_ [pluralProp "placeholder" (100 :: Int) ["one" `iprop` ("plural one" :: String), "other" `iprop` ("plural other" :: String)]]

        li_ ["id" $= "f-msg", "key" $= "f-msg"] $
            $(message "photos" "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}} {takenAgo}.")
                [ "name" $= "Neil Armstrong"
                , "numPhotos" @= (100 :: Int)
                , elementProperty "takenAgo" $ span_ ["id" $= "takenAgoSpan"] "years ago"
                ]

        li_ ["id" $= "f-msg-prop", "key" $= "f-msg-prop"] $
            input_ [ $(messageProp "placeholder" "photosprop" "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}}")
                [ "name" `iprop` ("Neil Armstrong" :: String)
                , "numPhotos" `iprop` (100 :: Int)
                ]
            ]

        li_ ["id" $= "f-msg-with-trans", "key" $= "f-msg-with-trans"] $
            $(message "with_trans" "this is not used {abc}") ["abc" $= "xxx"]

        li_ ["id" $= "f-msg-with-descr", "key" $= "f-msg-with-descr"] $
            $(message' "photos2" "How many photos?" "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}}.")
                [ "name" $= "Neil Armstrong"
                , "numPhotos" @= (0 :: Int)
                ]

        li_ ["id" $= "f-msg-prop-with-descr", "key" $= "f-msg-prop-with-descr"] $
            input_ [$(messageProp' "placeholder" "photosprop2" "How many photos?" "{name} took {numPhotos, number} photos")
                        [ "name" `iprop` ("Neil Armstrong" :: String)
                        , "numPhotos" `iprop` (0 :: Int)
                        ]
                   ]

        li_ ["id" $= "f-html-msg", "key" $= "f-html-msg"] $
            $(htmlMsg "html1" "<b>{num}</b> is the answer to life, the universe, and everything")
                [ "num" @= (42 :: Int) ]

        li_ ["id" $= "f-html-msg-with-descr", "key" $= "f-html-msg-with-descr"] $
            $(htmlMsg' "html2" "Hitchhiker's Guide" "{num} is the <b>answer</b> to life, the universe, and everything")
                [ "num" @= (42 :: Int) ]

--------------------------------------------------------------------------------
--- Main
--------------------------------------------------------------------------------

-- | Test a lifecycle view with all lifecycle methods nothing
testClient :: View '[]
testClient = mkView "app" $
  div_ $ do
    view_ eventsView "events"
    view_ storeSpec "store"
    view_ intlSpec "intl"
    view_ callbackViewWrapper "callback"

    div_ ["key" $= "raw"] $
      rawJsRendering js_testRawJs $
        span_ ["id" $= "test-raw-js-body", "key" $= "raw-body"]
          "Raw Javascript Render Body"

main :: IO ()
main = do
  initOutputStore
  initCharacterStore
  reactRenderView "app" testClient

#ifdef __GHCJS__

foreign import javascript unsafe
    "hsreact$log_message($1)"
    js_output :: JSString -> IO ()

foreign import javascript unsafe
    "React['createElement']('p', {'id': 'test-raw-js-para', 'key': 'test-raw-para'}, $2)"
    js_testRawJs :: JSVal -> JSArray -> IO JSVal

foreign import javascript unsafe
    "{'with_trans': 'message from translation {abc}'}"
    js_translations :: JSVal

#else

js_output :: JSString -> IO ()
js_output _ = error "js_output only works with GHCJS"

js_testRawJs :: JSVal -> JSArray -> IO JSVal
js_testRawJs _ _ = error "js_testRawJs only works with GHCJS"

js_translations :: JSVal
js_translations = error "js_translations only works with GHCJS"

#endif

writeIntlMessages (intlFormatJson "test/client/msgs/jsonmsgs.json")
writeIntlMessages (intlFormatJsonWithoutDescription "test/client/msgs/jsonnodescr.json")
writeIntlMessages (intlFormatAndroidXML "test/client/msgs/android.xml")
