-- | Make calls to the backend from within your stores.  This module is low-level
-- in that it mostly directly exposes the XMLHttpRequest access.  If you are using
-- servant, <http://hackage.haskell.org/package/react-flux-servant react-flux-servant>
-- for a higher-level interface.

{-# LANGUAGE CPP #-}

module React.Flux.Ajax (
    initAjax
  , RequestTimeout(..)
  , jsonAjax
  , AjaxRequest(..)
  , AjaxResponse(..)
  , ajax
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.String.Conversions (cs, (<>))
import GHC.Generics (Generic)
import React.Flux.Internal
import React.Flux.Store

import Control.Arrow ((***))
import Control.DeepSeq (deepseq)
import GHCJS.Foreign.Callback
import GHCJS.Foreign (jsNull)
import GHCJS.Foreign.Export
import GHCJS.Marshal (ToJSVal(..))
import GHCJS.Types (JSVal)
import qualified Data.JSString as JSS
import qualified Data.JSString.Text as JSS


-- | An optional timeout to use for @XMLHttpRequest.timeout@.
-- When a request times out, a status code of 504 is set in 'respStatus' and the
-- response handler executes.
data RequestTimeout = TimeoutMilliseconds Int | NoTimeout

instance ToJSVal RequestTimeout where
    toJSVal (TimeoutMilliseconds i) = toJSVal i
    toJSVal NoTimeout = pure jsNull

-- | The input to an AJAX request built using @XMLHttpRequest@.
data AjaxRequest = AjaxRequest
  { reqMethod :: JSString
  , reqURI :: JSString
  , reqTimeout :: RequestTimeout
  , reqHeaders :: [(JSString, JSString)]
  , reqBody :: JSString
  } deriving (Typeable, Generic)

-- | The response after @XMLHttpRequest@ indicates that the @readyState@ is done.
data AjaxResponse = AjaxResponse
  { respStatus :: Int
  , respResponseText :: JSString
  , respResponseXHR :: JSVal -- ^ The raw @XMLHttpRequest@ object.  Use this if you want more status about the response,
                             --   such as response headers.
  } deriving (Typeable, Generic)

-- | GHCJS panics if we export the function directly, so wrap it in a data struct
data HandlerWrapper = HandlerWrapper (AjaxResponse -> IO [SomeStoreAction])
    deriving Typeable

-- | This is broken out as a separate function because if the code is inlined (either manually or by
-- ghc), there is a runtime error.
useHandler :: AjaxResponse -> HandlerWrapper -> IO ()
useHandler r (HandlerWrapper h) = do
    actions <- h r
    actions `deepseq` mapM_ executeAction actions
{-# NOINLINE useHandler #-} -- if this is inlined, there is a bug in ghcjs

-- | If you are going to use 'ajax' or 'jsonAjax', you must call 'initAjax' once from your main
-- function.  The call should appear before the call to 'reactRender'.
initAjax :: IO ()
initAjax = do
    return ()
    a <- asyncCallback2 $ \rawXhr h -> do
        resp <- if js_reqTimedOut h
                  then return $ AjaxResponse 504 "Timeout" rawXhr
                  else AjaxResponse <$> js_xhrStatus rawXhr <*> js_xhrResponseText rawXhr <*> pure rawXhr
        e <- js_getHandlerWrapper h
        js_release e
        unsafeDerefExport "initAjax" e >>= useHandler resp

    js_setAjaxCallback a

-- | Use @XMLHttpRequest@ to send a request to the backend.  Once the response arrives
-- and the @readyState@ is done, the response will be passed to the given handler and the resulting
-- actions will be executed.  Note that 'ajax' returns immedietly and does not wait for the request
-- to finish.
ajax :: AjaxRequest -> (AjaxResponse -> IO [SomeStoreAction]) -> IO ()
ajax req handler = do
    reqJson <- toJSVal req
    handlerE <- export $ HandlerWrapper handler
    js_ajax reqJson handlerE

-- | Use @XMLHttpRequest@ to send a request with a JSON body, parse the response body as JSON, and
-- then dispatch some actions with the response.  This should be used from within the transform
-- function of your store.  For example,
--
-- >data Target = AlienShip | AlienPlanet
-- >  deriving (Show, Typeable, Generic, ToJSON, FromJSON)
-- >
-- >data Trajectory = Trajectory
-- >    { x :: Double, y :: Double, z :: Double, vx :: Double, vy :: Double, vz :: Double }
-- >  deriving (Show, Typeable, Generic, ToJSON, FromJSON)
-- >
-- >data UpdatePending = NoUpdatePending | UpdatePending Text | PreviousUpdateHadError Text
-- >
-- >data MyStore = MyStore { currentTrajectory :: Maybe Trajectory, launchUpdate :: UpdatePending }
-- >
-- >data MyStoreAction = LaunchTheMissiles Target
-- >                   | MissilesLaunched Trajectory
-- >                   | UnableToLaunchMissiles Text
-- >  deriving (Typeable, Generic, NFData)
-- >
-- >instance StoreData MyStore where
-- >    type StoreAction MyStore = MyStoreAction
-- >
-- >    transform (LaunchTheMissiles t) s = do
-- >        jsonAjax NoTimeout "PUT" "/launch-the-missiles" [] t $ \case
-- >            Left (_, msg) -> return [SomeStoreAction myStore $ UnableToLaunchMissiles msg]
-- >            Right traj -> return [SomeStoreAction myStore $ MissilesLaunched traj]
-- >        return s { launchUpdate = UpdatePending ("Requesting missle launch against " ++ T.pack (show t)) }
-- >
-- >    transform (MissilesLaunched traj) s =
-- >        return s { currentTrajectory = Just traj, launchUpdate = NoUpdatePending }
-- >
-- >    transform (UnableToLaunchMissiles err) s =
-- >        return s { launchUpdate = PreviousUpdateHadError err }
-- >
-- >myStore :: ReactStore MyStore
-- >myStore = unsafeMkStore $ MyStore Nothing NoUpdatePending
--
-- And then in your view, you can render this using something like:
--
-- >myView :: ReactView ()
-- >myView = defineControllerView "launch the missles" myStore $ \s () -> do
-- >    case launchUpdate s of
-- >        NoUpdatePending -> return ()
-- >        UpdatePending msg -> span_ $ faIcon_ "rocket" <> elemText msg
-- >        PreviousUpdateHadErroer err -> span_ $ faIcon_ "exclamation" <> elemText err
-- >    clbutton_ ["pure-button button-success"] ([SomeStoreAction myStore $ LaunchTheMissiles AlienShip]) $ do
-- >        faIcon_ "rocket"
-- >        "Launch the missles against the alien ship!"
-- >    p_ $ elemString $ "Current trajectory " ++ show (currentTrajectory s)
--
jsonAjax :: (ToJSON body, FromJSON response)
         => RequestTimeout
         -> Text -- ^ the method
         -> Text -- ^ the URI
         -> [(Text, Text)] -- ^ the headers.  In addition to these headers, 'jsonAjax' adds two headers:
                           -- @Content-Type: application/json@ and @Accept: application/json@.
         -> body -- ^ the body
         -> (Either (Int, Text) response -> IO [SomeStoreAction])
            -- ^ Once @XMLHttpRequest@ changes the @readyState@ to done this handler will be
            -- executed and the resulting actions dispatched to the stores.
            --
            --   * If the response status is @200@, the body will be parsed as JSON and a
            --     'Right' value will be passed to this handler.   If there is an
            --     error parsing the JSON response, a 'Left' value with @500@ and the error message
            --     from aeson is given to the handler.
            --
            --   * If the response status is anything besides @200@, a 'Left' value with a pair
            --     of the response status and response text is passed to the handler.
         -> IO ()
jsonAjax timeout method uri headers body handler = do
    let extraHeaders = [("Content-Type", "application/json"), ("Accept", "application/json")]
    let req = AjaxRequest
              { reqMethod = JSS.textToJSString method
              , reqURI = JSS.textToJSString uri
              , reqHeaders = extraHeaders ++ map (JSS.textToJSString *** JSS.textToJSString) headers
              , reqTimeout = timeout
              , reqBody = JSS.pack . cs . encode $ body
              }
    ajax req $ \resp ->
        if respStatus resp < 300
            then do
                case eitherDecode . cs . JSS.unpack . respResponseText $ resp of
                    Left err -> handler $ Left (500, "Unable to convert response body: " <> cs err)  -- TODO: this is not an HTTP error.
                    Right v  -> handler $ Right v
            else handler $ Left (respStatus resp, JSS.textFromJSString $ respResponseText resp)

instance ToJSVal AjaxRequest

#ifdef __GHCJS__

foreign import javascript unsafe
    "hsreact$ajax($1, $2)"
    js_ajax :: JSVal -> Export HandlerWrapper -> IO ()

foreign import javascript unsafe
    "hsreact$setAjaxCallback($1)"
    js_setAjaxCallback :: Callback (JSVal -> JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
    "$1.hs"
    js_getHandlerWrapper :: JSVal -> IO (Export HandlerWrapper)

foreign import javascript unsafe
    "$r = $1.timedOut"
    js_reqTimedOut :: JSVal -> Bool

foreign import javascript unsafe
    "$1['status']"
    js_xhrStatus :: JSVal -> IO Int

foreign import javascript unsafe
    "$1['responseText']"
    js_xhrResponseText :: JSVal -> IO JSString

foreign import javascript unsafe
    "h$release($1)"
    js_release :: Export HandlerWrapper -> IO ()

#else

js_ajax :: JSVal -> Export HandlerWrapper -> IO ()
js_ajax _ _ = error "js_ajax only works with GHCJS"

js_setAjaxCallback :: Callback (JSVal -> JSVal -> IO ()) -> IO ()
js_setAjaxCallback _ = error "js_setAjaxCallback only works with GHCJS"

js_getHandlerWrapper :: JSVal -> IO (Export HandlerWrapper)
js_getHandlerWrapper _ = error "js_getHandlerWrapper only works with GHCJS"

js_reqTimedOut :: JSVal -> Bool
js_reqTimedOut _ = error "js_reqTimedOut only works with GHCJS"

js_xhrStatus :: JSVal -> IO Int
js_xhrStatus _ = error "js_xhrStatus only works with GHCJS"

js_xhrResponseText :: JSVal -> IO JSString
js_xhrResponseText _ = error "js_xhrResponseText only works with GHCJS"

js_release :: Export HandlerWrapper -> IO ()
js_release _ = error "js_release only works with GHCJS"

#endif
