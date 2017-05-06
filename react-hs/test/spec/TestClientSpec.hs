{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module TestClientSpec (spec) where

import           Control.Monad
import           Data.Monoid ((<>))
import           Control.Monad.IO.Class (liftIO)
import           Data.List
import           Data.Time
import qualified Data.Text              as T
import           Test.Hspec.WebDriver
import           Test.WebDriver.Capabilities

tshow :: Show a => a -> T.Text
tshow = T.pack . show

loadLog :: WD [String]
loadLog = executeJS [] "var old = window.test_client_output; window.test_client_output = []; return old;"

shouldBeEvent :: String -> (String, Bool, Int) -> WD ()
shouldBeEvent evt (expectedType, evtBandC, evtPhase) = do
    let prefix = concat
                  [ "Event {evtType = \"" ++ expectedType ++ "\", "
                  , "evtBubbles = " ++ show evtBandC ++ ", "
                  , "evtCancelable = " ++ show evtBandC ++ ", "
                  , "evtCurrentTarget = EventTarget, evtDefaultPrevented = False, "
                  , "evtPhase = " ++ show evtPhase ++ ", "
                  , "evtIsTrusted = True, evtTarget = EventTarget, evtTimestamp = "
                  ]
    unless (prefix `isPrefixOf` evt) $
        error $ "Expecting " ++ prefix ++ " but got " ++ evt
    let suffix = dropWhile (/= ' ') $ drop (length prefix) evt
    suffix `shouldBe` " evtHandlerArg = HandlerArg}"

newtype CharacterUL = CharacterUL { characterLis :: [Element] }

characterList :: T.Text -> WD CharacterUL
characterList ulId = CharacterUL <$> findElems (ByCSS $ "ul#" <> ulId <> " > li")

characterShouldBe :: CharacterUL -> Int -> T.Text -> WD ()
characterShouldBe (CharacterUL es) idx msg = do
  let e = es !! idx
  getText e `shouldReturn` msg

clickNoChangeHuman :: WD ()
clickNoChangeHuman = findElem (ById "Humans-none") >>= click

clickNoChangeTiste :: WD ()
clickNoChangeTiste = findElem (ById "Tiste-none") >>= click

data HumanNames = QuickBen | Whiskeyjack | Fiddler | Kruppe
data TisteNames = Andarist | Osseric | AnomanderRake | Korlot

incrementHuman :: HumanNames -> WD ()
incrementHuman n = findElem (ById i) >>= click
  where
    i = "Humans-" <> case n of
      QuickBen -> "P1_C1"
      Whiskeyjack -> "P1_C2"
      Fiddler -> "P2_C1"
      Kruppe -> "P2_C2"

incrementTiste :: TisteNames -> WD ()
incrementTiste n = findElem (ById i) >>= click
  where
    i = "Tiste-" <> case n of
      Andarist -> "P1_C1"
      Osseric -> "P1_C2"
      AnomanderRake -> "P2_C1"
      Korlot -> "P2_C2"

cUpdate :: T.Text -> T.Text -> String
cUpdate ulId msg = T.unpack $ "Update in ul " <> ulId <> ": " <> msg

data TisteNums = TisteNums { andarist :: Int, osseric :: Int, rake :: Int, korlot :: Int, rakeState :: Int}
data HumanNums = HumanNums { quickBen :: Int, whiskeyjack :: Int, fiddler :: Int, kruppe :: Int}

charactersShouldBe :: HumanNums -> TisteNums -> WD ()
charactersShouldBe (HumanNums{..}) (TisteNums{..}) = do
  allTiste <- characterList "tiste-sub-view"
  characterShouldBe allTiste 0 "All the tiste"
  characterShouldBe allTiste 1 $ "Single character C" <> tshow andarist <> " - Andarist"
  characterShouldBe allTiste 2 $ "Single character C" <> tshow osseric <> " - Osseric"
  characterShouldBe allTiste 3 $ "Single character C" <> tshow rake <> " - Anomander Rake"
  characterShouldBe allTiste 4 $ "Single character C" <> tshow korlot <> " - Korlot"

  allHumans <- characterList "full-humans-view"
  characterShouldBe allHumans 0 "All the humans, plus Andarist and Rake"
  characterShouldBe allHumans 1 $ "Single character C" <> tshow quickBen <> " - Quick Ben"
  characterShouldBe allHumans 2 $ "Single character C" <> tshow whiskeyjack <> " - Whiskeyjack"
  characterShouldBe allHumans 3 $ "Single character C" <> tshow fiddler <> " - Fiddler"
  characterShouldBe allHumans 4 $ "Single character C" <> tshow kruppe <> " - Kruppe"
  characterShouldBe allHumans 5 $ "Two characters C" <> tshow quickBen <> " - Quick Ben and C" <> tshow whiskeyjack <> " - Whiskeyjack"
  characterShouldBe allHumans 6 $ "Pair of characters C" <> tshow fiddler <> " - Fiddler, C" <> tshow kruppe <> " - Kruppe"
  characterShouldBe allHumans 7 $ "Single character C" <> tshow andarist <> " - Andarist"
  characterShouldBe allHumans 8 $ "Single character C" <> tshow rake <> " - Anomander Rake"

  both <- characterList "dual-character-view"
  characterShouldBe both 0 "Quick Ben and Andarist"
  characterShouldBe both 1 $ "Single character C" <> tshow quickBen <> " - Quick Ben"
  characterShouldBe both 2 $ "Single character C" <> tshow andarist <> " - Andarist"
  stateRake <- findElemFrom (characterLis both !! 3) $ ByCSS "p > span:first-child"
  getText stateRake `shouldReturn` ("Stateful character C" <> tshow rake <> " - Anomander Rake")
  stateSpan <- findElemFrom (characterLis both !! 3) $ ByCSS "p > span:nth-child(2)"
  getText stateSpan `shouldReturn` (tshow rakeState)

  some <- characterList "tiste-and-some-humans"
  characterShouldBe some 0 "Just Rake, Korlot, Quick Ben, and Whiskeyjack"
  characterShouldBe some 1 $ "Single character C" <> tshow rake <> " - Anomander Rake"
  characterShouldBe some 2 $ "Single character C" <> tshow korlot <> " - Korlot"
  characterShouldBe some 3 $ "Single character C" <> tshow quickBen <> " - Quick Ben"
  characterShouldBe some 4 $ "Single character C" <> tshow whiskeyjack <> " - Whiskeyjack"

intlSpanShouldBe :: String -> String -> WD ()
intlSpanShouldBe ident txt = do
    e <- findElem (ById $ T.pack ident)
    getText e `shouldReturn` T.pack txt

intlPlaceholderShouldBe :: String -> String -> WD ()
intlPlaceholderShouldBe ident txt = do
    e <- findElem (ById $ T.pack ident)
    input <- findElemFrom e $ ByTag "input"
    (input `attr` "placeholder") `shouldReturn` Just (T.pack txt)

-- | Only up to 999,999 since this is just used for the number of days since 1969
showWithComma :: Integer -> String
showWithComma i = show x ++ "," ++ replicate (3-length y') '0' ++ y'
    where
        (x, y) = divMod i 1000
        y' = show y

spec :: Spec
spec = do
  testClientSpec 8087 "test-client.html"
  testClientSpec 8087 "test-client-min.html"

allBrowsers :: [Capabilities]
allBrowsers = [defaultCaps]

testClientSpec :: Int -> String -> Spec
testClientSpec port filename = session (" for the test client " ++ filename) $ using allBrowsers $ do
    let appurl = "http://localhost:" ++ show port ++ "/" ++ filename
    it ("opens " ++ show appurl) $ runWD $ do
        openPage appurl

    describe "Events" $ do
      it "processes a focus event" $ runWD $ do
          findElem (ById "keyinput") >>= click
          [focus, evt] <- loadLog
          focus `shouldBe` "focus"
          evt `shouldBeEvent` ("focus", False, 1)

      it "processes a keydown event" $ runWD $ do
          findElem (ById "keyinput") >>= sendKeys "x"
          [keydown, evt, keyEvt, modState, target, curTarget] <- loadLog
          keydown `shouldBe` "keydown"
          evt `shouldBeEvent` ("keydown", True, 3)
          keyEvt `shouldBe` "(False,0,False,\"x\",88,Nothing,0,False,False,False,88)"
          modState `shouldBe` "alt modifier: False"
          target `shouldBe` "keyinput"
          curTarget `shouldBe` "keyinput"

      it "processes a keydown with alt" $ runWD $ do
          findElem (ById "keyinput") >>= sendKeys "\xE00Ar" -- send Alt-r
          -- generates two events, one for alt, one for r
          [_, _, keyEvt, modState, _, _, _, _, keyEvt2, modState2, _, _] <- loadLog
          keyEvt `shouldBe` "(True,0,False,\"Alt\",18,Nothing,0,False,False,False,18)"
          modState `shouldBe` "alt modifier: True"
          keyEvt2 `shouldBe` "(True,0,False,\"r\",82,Nothing,0,False,False,False,82)"
          modState2 `shouldBe` "alt modifier: True"

      it "processes a click event" $ runWD $ do
          findElem (ById "clickinput") >>= click
          [clickName, evt, mouseEvt, modState] <- loadLog
          clickName `shouldBe` "click"
          evt `shouldBeEvent` ("click", True, 3)
          mouseEvt `shouldBe` "(False,0,0,37,54,False,False,37,54,EventTarget,37,54,False)"
          modState `shouldBe` "alt modifier: False"

      {- touch events can't be tested at the moment, chrome doesn't support them
      it "processes a touchinput event" $ runWD $ do
          t <- findElem $ ById "touchinput"
          touchClick t
      -}

      it "stops the default browser action" $ runWD $ do
          findElem (ById "some-link") >>= click
          [x] <- loadLog
          x `shouldBe` "Click some-link"
          url <- getCurrentURL
          unless ("file:" `isPrefixOf` url) $ error "Default browser action was not prevented"

      it "stops propagating an event in the bubbling phase" $ runWD $ do
          findElem (ById "inner-span") >>= moveToCenter
          clickWith LeftButton
          [inner] <- loadLog
          inner `shouldBe` "Click inner span"

      it "stops propagating an event during the capture phase" $ runWD $ do
          findElem (ById "inner-span") >>= moveToCenter
          doubleClick
          [inner, outer] <- loadLog
          inner `shouldBe` "Click inner span"
          outer `shouldBe` "Double click outer div"

    describe "Stores and Views" $ do
      it "shows the initial tiste and humans" $ runWD $
        charactersShouldBe
          (HumanNums { quickBen = 10, whiskeyjack = 20, fiddler = 30, kruppe = 40})
          (TisteNums { andarist = 100, osseric = 110, rake = 120, korlot = 130, rakeState = -100})

      it "does nothing when clicking no-change button" $ runWD $ do
        clickNoChangeHuman
        charactersShouldBe
          (HumanNums { quickBen = 10, whiskeyjack = 20, fiddler = 30, kruppe = 40})
          (TisteNums { andarist = 100, osseric = 110, rake = 120, korlot = 130, rakeState = -100})
        loadLog `shouldReturn` []

        clickNoChangeTiste
        charactersShouldBe
          (HumanNums { quickBen = 10, whiskeyjack = 20, fiddler = 30, kruppe = 40})
          (TisteNums { andarist = 100, osseric = 110, rake = 120, korlot = 130, rakeState = -100})
        loadLog `shouldReturn` []

      it "increments Quick Ben" $ runWD $ do
        incrementHuman QuickBen
        charactersShouldBe
          (HumanNums { quickBen = 11, whiskeyjack = 20, fiddler = 30, kruppe = 40})
          (TisteNums { andarist = 100, osseric = 110, rake = 120, korlot = 130, rakeState = -100})
        loadLog `shouldReturn`
          [ cUpdate "full-humans-view" "All the humans, plus Andarist and Rake"
          , cUpdate "full-humans-view" "Single character C10 - Quick Ben"
          , cUpdate "full-humans-view" "Two characters C10 - Quick Ben and C20 - Whiskeyjack"
          , cUpdate "dual-character-view" "Quick Ben and Andarist"
          , cUpdate "dual-character-view" "Single character C10 - Quick Ben"
          , cUpdate "tiste-and-some-humans" "Just Rake, Korlot, Quick Ben, and Whiskeyjack"
          , cUpdate "tiste-and-some-humans" "Single character C10 - Quick Ben"
          ]

      it "increments Whiskeyjack" $ runWD $ do
        incrementHuman Whiskeyjack
        charactersShouldBe
          (HumanNums { quickBen = 11, whiskeyjack = 21, fiddler = 30, kruppe = 40})
          (TisteNums { andarist = 100, osseric = 110, rake = 120, korlot = 130, rakeState = -100})
        loadLog `shouldReturn`
          [ cUpdate "full-humans-view" "All the humans, plus Andarist and Rake"
          , cUpdate "full-humans-view" "Single character C20 - Whiskeyjack"
          , cUpdate "full-humans-view" "Two characters C11 - Quick Ben and C20 - Whiskeyjack"
          , cUpdate "dual-character-view" "Quick Ben and Andarist" -- just the view itself, not any sub-views
          , cUpdate "tiste-and-some-humans" "Just Rake, Korlot, Quick Ben, and Whiskeyjack"
          , cUpdate "tiste-and-some-humans" "Single character C20 - Whiskeyjack"
          ]

      it "increments Fiddler" $ runWD $ do
        incrementHuman Fiddler
        charactersShouldBe
          (HumanNums { quickBen = 11, whiskeyjack = 21, fiddler = 31, kruppe = 40})
          (TisteNums { andarist = 100, osseric = 110, rake = 120, korlot = 130, rakeState = -100})
        loadLog `shouldReturn`
          [ cUpdate "full-humans-view" "All the humans, plus Andarist and Rake"
          , cUpdate "full-humans-view" "Single character C30 - Fiddler"
          , cUpdate "full-humans-view" "Pair of characters C30 - Fiddler, C40 - Kruppe"
          , cUpdate "dual-character-view" "Quick Ben and Andarist" -- just the view itself, not any sub-views

          -- tiste-and-some-humans should have nothing, even though the human store is there,
          -- because only the first two humans are extracted before shouldComponentUpdate
          ]

      it "increments Kruppe" $ runWD $ do
        incrementHuman Kruppe
        charactersShouldBe
          (HumanNums { quickBen = 11, whiskeyjack = 21, fiddler = 31, kruppe = 41})
          (TisteNums { andarist = 100, osseric = 110, rake = 120, korlot = 130, rakeState = -100})
        loadLog `shouldReturn`
          [ cUpdate "full-humans-view" "All the humans, plus Andarist and Rake"
          , cUpdate "full-humans-view" "Single character C40 - Kruppe"
          , cUpdate "full-humans-view" "Pair of characters C31 - Fiddler, C40 - Kruppe"
          , cUpdate "dual-character-view" "Quick Ben and Andarist" -- just the view itself, not any sub-views

          -- tiste-and-some-humans should have nothing, even though the human store is there,
          -- because only the first two humans are extracted before shouldComponentUpdate
          ]

      it "does nothing when clicking no-change button" $ runWD $ do
        clickNoChangeHuman
        charactersShouldBe
          (HumanNums { quickBen = 11, whiskeyjack = 21, fiddler = 31, kruppe = 41})
          (TisteNums { andarist = 100, osseric = 110, rake = 120, korlot = 130, rakeState = -100})
        loadLog `shouldReturn` []

        clickNoChangeTiste
        charactersShouldBe
          (HumanNums { quickBen = 11, whiskeyjack = 21, fiddler = 31, kruppe = 41})
          (TisteNums { andarist = 100, osseric = 110, rake = 120, korlot = 130, rakeState = -100})
        loadLog `shouldReturn` []

      it "increments Andarist" $ runWD $ do
        incrementTiste Andarist
        charactersShouldBe
          (HumanNums { quickBen = 11, whiskeyjack = 21, fiddler = 31, kruppe = 41})
          (TisteNums { andarist = 101, osseric = 110, rake = 120, korlot = 130, rakeState = -100})
        loadLog `shouldReturn`
          [ cUpdate "tiste-sub-view" "All the tiste"
          , cUpdate "tiste-sub-view" "Single character C100 - Andarist"

          -- Andarist is a parameter to the human view
          , cUpdate "full-humans-view" "All the humans, plus Andarist and Rake"
          , cUpdate "full-humans-view" "Single character C100 - Andarist"

          , cUpdate "dual-character-view" "Quick Ben and Andarist"
          , cUpdate "dual-character-view" "Single character C100 - Andarist"

          , cUpdate "tiste-and-some-humans" "Just Rake, Korlot, Quick Ben, and Whiskeyjack" -- just the view, no subviews
          ]

      it "increments Osseric" $ runWD $ do
        incrementTiste Osseric
        charactersShouldBe
          (HumanNums { quickBen = 11, whiskeyjack = 21, fiddler = 31, kruppe = 41})
          (TisteNums { andarist = 101, osseric = 111, rake = 120, korlot = 130, rakeState = -100})
        loadLog `shouldReturn`
          [ cUpdate "tiste-sub-view" "All the tiste"
          , cUpdate "tiste-sub-view" "Single character C110 - Osseric"
          -- nothing to full-humans-view
          , cUpdate "dual-character-view" "Quick Ben and Andarist" -- just the view, no subviews
          , cUpdate "tiste-and-some-humans" "Just Rake, Korlot, Quick Ben, and Whiskeyjack" -- just the view, no subviews
          ]

      it "increments Anomander Rake" $ runWD $ do
        incrementTiste AnomanderRake
        charactersShouldBe
          (HumanNums { quickBen = 11, whiskeyjack = 21, fiddler = 31, kruppe = 41})
          (TisteNums { andarist = 101, osseric = 111, rake = 121, korlot = 130, rakeState = -100})
        loadLog `shouldReturn`
          [ cUpdate "tiste-sub-view" "All the tiste"
          , cUpdate "tiste-sub-view" "Single character C120 - Anomander Rake"
          , cUpdate "full-humans-view" "All the humans, plus Andarist and Rake"
          , cUpdate "full-humans-view" "Single character C120 - Anomander Rake"
          , cUpdate "dual-character-view" "Quick Ben and Andarist"
          , cUpdate "dual-character-view" "Stateful character C120 - Anomander Rake"
          , cUpdate "tiste-and-some-humans" "Just Rake, Korlot, Quick Ben, and Whiskeyjack"
          , cUpdate "tiste-and-some-humans" "Single character C120 - Anomander Rake"
          ]

      it "increments Korlot" $ runWD $ do
        incrementTiste Korlot
        charactersShouldBe
          (HumanNums { quickBen = 11, whiskeyjack = 21, fiddler = 31, kruppe = 41})
          (TisteNums { andarist = 101, osseric = 111, rake = 121, korlot = 131, rakeState = -100})
        loadLog `shouldReturn`
          [ cUpdate "tiste-sub-view" "All the tiste"
          , cUpdate "tiste-sub-view" "Single character C130 - Korlot"
          -- nothing for the humans view
          , cUpdate "dual-character-view" "Quick Ben and Andarist"
          , cUpdate "tiste-and-some-humans" "Just Rake, Korlot, Quick Ben, and Whiskeyjack"
          , cUpdate "tiste-and-some-humans" "Single character C130 - Korlot"
          ]

      it "increments Anomander Rake's state" $ runWD $ do
        findElem (ByCSS "button.incr-state") >>= click
        charactersShouldBe
          (HumanNums { quickBen = 11, whiskeyjack = 21, fiddler = 31, kruppe = 41})
          (TisteNums { andarist = 101, osseric = 111, rake = 121, korlot = 131, rakeState = -99})
        loadLog `shouldReturn`
          [ cUpdate "dual-character-view" "Stateful character C121 - Anomander Rake"
          ]

    describe "i18n" $ do
        it ("opens " ++ show appurl) $ runWD $ do
            openPage appurl

        it "displays the intl formatted data" $ runWD $ do
            "f-number" `intlSpanShouldBe` "90%"
            "f-int" `intlSpanShouldBe` "100,000"
            "f-double" `intlSpanShouldBe` "40,000.2"
            "f-shortday" `intlSpanShouldBe` "Jul 20, 1969"
            "f-fullday" `intlSpanShouldBe` "Sunday, July 20, 69 AD"
            "f-date" `intlSpanShouldBe` "Sun, Jul 20, 69"
            -- f-shorttime and f-fulltime cannot be (easily) tested since they rely on the current timezone
            "f-time" `intlSpanShouldBe` "Jul 19, 69, 4:56:00 PM GMT-10"
            "f-plural" `intlSpanShouldBe` "plural other"

            today <- liftIO (utctDay <$> getCurrentTime)
            let moon = fromGregorian 1969 7 20
                daysAgo = diffDays today moon
                yearsAgo :: Int = round $ realToFrac daysAgo / (365 :: Double) -- is close enough
            "f-relative" `intlSpanShouldBe` (show (yearsAgo) ++ " years ago")
            "f-relative-days" `intlSpanShouldBe` (showWithComma (daysAgo+1) ++ " days ago")

        it "displays messages" $ runWD $ do
            msg <- findElem $ ById "f-msg"
            getText msg `shouldReturn` "Neil Armstrong took 100 photos years ago."
            takenAgoSpan <- findElemFrom msg $ ById "takenAgoSpan"
            getText takenAgoSpan `shouldReturn` "years ago"

            msg' <- findElem $ ById "f-msg-with-descr"
            getText msg' `shouldReturn` "Neil Armstrong took no photos."

            "f-msg-with-trans" `intlSpanShouldBe` "message from translation xxx"

            htmlMsg <- findElem $ ById "f-html-msg"
            getText htmlMsg `shouldReturn` "42 is the answer to life, the universe, and everything"
            (findElemFrom htmlMsg (ByTag "b") >>= getText)
                `shouldReturn` "42"

            htmlMsg' <- findElem $ ById "f-html-msg-with-descr"
            getText htmlMsg' `shouldReturn` "42 is the answer to life, the universe, and everything"
            (findElemFrom htmlMsg' (ByTag "b") >>= getText)
                `shouldReturn` "answer"

        it "displays formatted properties" $ runWD $ do
            "f-number-prop" `intlPlaceholderShouldBe` "123,456"
            "f-date-prop" `intlPlaceholderShouldBe` "7/20/1969"
            "f-time-prop" `intlPlaceholderShouldBe` "Jul 19, 69, 4 PM"
            "f-plural-prop" `intlPlaceholderShouldBe` "other"
            "f-msg-prop" `intlPlaceholderShouldBe` "Neil Armstrong took 100 photos"
            "f-msg-prop-with-descr" `intlPlaceholderShouldBe` "Neil Armstrong took 0 photos"

    describe "Callbacks and Raw" $ do
      it "has rendered the raw show view" $ runWD $
          (findElem (ById "raw-show-view") >>= getText)
              `shouldReturn` "42"

      it "has rendered the raw javascript rendering" $ runWD $
          (findElem (ByCSS "p#test-raw-js-para > span") >>= getText)
              `shouldReturn` "Raw Javascript Render Body"

      it "renders a callback returning a view" $ runWD $ do
          e <- findElem $ ById "callback-view-props-test"
          getText e `shouldReturn` "Props are 5 and Hello World"
