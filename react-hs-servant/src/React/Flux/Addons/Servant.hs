-- | Make calls to an API defined using "Servant.API" from your "React.Flux" stores.
-- __You must call 'initAjax' from your main function for this to work.__
-- For now, this module only works with JSON.
--
-- The general pattern I currently use is to create a store action to trigger the request
-- and another store action to process the response.  Only the store action to trigger
-- the request is exported.  This does lead to a bit of tedious boilerplate since you need
-- actions for each request, so I am still searching for a better API that perhaps takes
-- advantage of the servant API type-level computation to reduce the boilerplate.  For now,
-- until I figure out a better way, this direct approach does work.  If you have any ideas
-- for a good API, please open an issue on bitbucket (even if you don't have full code)!
--
-- As an example, say that the API consists of two methods:
--
-- @
-- type GetUser = "user" :> Capture "user_id" UserId :> Get '[JSON] User
-- type SetUser = "user" :> Capture "user_id" UserId :> ReqBody '[JSON] User :> Post '[JSON] ()
-- type MyAPI = GetUser :\<|\> SetUser
-- @
--
-- I would create a store as follows:
--
-- @
-- data RequestStatus = NoPendingRequest | PendingRequest | PreviousRequestHadError String
--
-- data UserStore = UserStore
--   { users :: Map.HashMap UserId User
--   , reqStatus :: RequestStatus
--   }
--
-- data UserStoreAction = RequestUser UserId
--                      | RequestUserResponse UserId (Either (Int,String) User)
--                      | UpdateUser UserId User
--                      | UpdateUserResponse UserId (Either (Int, String) ())
--   deriving (Show, Generic, NFData)
--
-- cfg :: ApiRequestConfig MyAPI
-- cfg = ApiRequestConfig "https://www.example.com" NoTimeout
--
-- instance StoreData UserStore where
--   type StoreAction UserStore = UserStoreAction
--
--   transform (RequestUser uid) us = do
--     request cfg (Proxy :: Proxy GetUser) uid $
--       \\r -> return [SomeStoreAction userStore $ RequestUserResponse uid r]
--     return $ us {reqStatus = PendingRequest}
--
--   transform (RequestUserResponse _ (Left (_errCode, err))) us =
--     return $ us {reqStatus = PreviousRequestHadError err}
--
--   transform (RequestUserResponse uid (Right u)) us =
--     return $ us { reqStatus = NoPendingRequest
--                 , users = Map.insert uid u (users us)
--                 }
--
--   transform (UpdateUser uid u) us = do
--     request cfg (Proxy :: Proxy SetUser) uid u $
--       \\r -> return [SomeStoreAction userStore $ UpdateUserResponse uid r]
--     return $ us { reqStatus = PendingRequest
--                 , users = Map.insert uid u (users us)
--                 }
--
--   transform (UpdateUserResponse uid (Left (_errCode, err))) us =
--     return $ us { reqStatus = PreviousRequestHadError err
--                 , users = Map.delete uid (users us)
--                 }
--
--   transform (UpdateUserResponse _ (Right ())) us =
--     return $ us { reqStatus = NoPendingRequest}
--
-- userStore :: ReactStore UserStore
-- userStore = mkStore $ UserStore Map.empty NoPendingRequest
-- @
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module React.Flux.Addons.Servant(
    HandleResponse
  , RequestTimeout(..)
  , ApiRequestConfig(..)
  , request
  , HasAjaxRequest(..)
  , Request(..)
) where

import React.Flux
import React.Flux.Ajax
import Servant.Utils.Links
import Servant.API
import GHC.TypeLits
import Data.String.Conversions (cs)
import Data.Typeable (Proxy(..))
import Data.Aeson
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.JSString (JSString)
import Data.JSString.Text (textToJSString)
import qualified Data.JSString as JSS

jsPack :: String -> JSString
jsPack = JSS.pack
jsUnpack :: JSString -> String
jsUnpack = JSS.unpack
jsIntercalate :: JSString -> [JSString] -> JSString
jsIntercalate = JSS.intercalate


-- | Internal state used when building the request.
data Request = Request {
    segments :: [JSString]
  , rHeaders :: [(JSString, JSString)]
  , rQuery :: [(JSString, JSString)]
  , rBody :: JSString
  , rTimeout :: RequestTimeout
}

-- | When a response from the server arrives, it is either an error or a valid response.  An error
-- is turned into a 'Left' value consisting of the HTTP response code and the response body.  If
-- a 200 HTTP response is received, it is parsed to a value according to the servant API definition
-- and passed as a 'Right' value.  You must then turn the response into a store action.  I suggest
-- that you just pass the value along directly to a store action without any computation; the computation
-- can happen inside the store.
type HandleResponse a = Either (Int,String) a -> IO [SomeStoreAction]

-- | Settings for requests built using this module.
data ApiRequestConfig api = ApiRequestConfig
    { urlPrefix :: JSString
    -- ^ A prefix for all requests; it should include the domain and any prefix path that is required.
    -- To this prefix a forward slash is appended and then the path built using the Servant API definition.
    , timeout :: RequestTimeout
    -- ^ The timeout to use for requests.  If a timeout occurs, a 'Left' value with code 504 is passed to
    -- 'HandleResponse'.
    }

-- | Make a request to a servant endpoint. __You must call 'initAjax' from your main function for this to work.__
--
-- 'request' takes the 'ApiRequestConfig', a proxy for the endpoint,
-- parameters for the request (request body, query params, path captures, etc.), and value of type 'HandleResponse'.
-- The result of 'request' is then a value of type @IO ()@.  In order to type-check that the proper values for
-- the request body, path captures, etc. are passed, the 'MkRequest' associated type is used.  'MkRequest' expands
-- to a function with one argument for each path piece and an argument for the 'HandleResponse'.  For example,
--
-- @
-- type GetUser = "user" :> Capture "user_id" UserId :> Get '[JSON] User
-- type SetUser = "user" :> Capture "user_id" UserId :> ReqBody '[JSON] User :> Post '[JSON] ()
-- type MyAPI = GetUser :\<|\> SetUser
-- @
--
-- Then
--
-- @
-- MkRequest GetUser ~ UserId -> HandleResponse User -> IO ()
-- MkRequest SetUser ~ UserId -> User -> HandleResponse () -> IO ()
-- @
--
-- so that
--
-- @
-- request cfg (Proxy :: Proxy GetUser) :: UserId -> HandleResponse User -> IO ()
-- request cfg (Proxy :: Proxy SetUser) :: UserId -> User -> HandleResponse () -> IO ()
-- @
request :: (IsElem endpoint api, HasAjaxRequest endpoint) => ApiRequestConfig api -> Proxy endpoint -> MkRequest endpoint
request (ApiRequestConfig p t) endpoint = toRequest endpoint (Request [p] [] [] "" t)

-- | A class very similar to "Servant.Utils.Links".  You shouldn't need to use this class directly: instead
-- use 'request'.  Having said that, the 'MkRequest' type defined in this typeclass is important as it determines
-- what values you must pass to 'request' to obtain a proper request.
class HasAjaxRequest endpoint where
    type MkRequest endpoint
    toRequest :: Proxy endpoint -> Request -> MkRequest endpoint

instance (ToJSON a, HasAjaxRequest sub) => HasAjaxRequest (ReqBody '[JSON] a :> sub) where
    type MkRequest (ReqBody '[JSON] a :> sub) = a -> MkRequest sub
    toRequest _ r body = toRequest (Proxy :: Proxy sub) (r
        { rBody = jsPack . cs $ encode body
        , rHeaders = rHeaders r ++ [("Content-Type", "application/json")]
        })

instance (KnownSymbol sym, HasAjaxRequest sub) => HasAjaxRequest (sym :> sub) where
    type MkRequest (sym :> sub) = MkRequest sub
    toRequest _ r = toRequest (Proxy :: Proxy sub) (r { segments = segments r ++ [seg]})
        where
            seg = jsPack $ symbolVal (Proxy :: Proxy sym)

instance (ToHttpApiData v, HasAjaxRequest sub) => HasAjaxRequest (Capture sym v :> sub) where
    type MkRequest (Capture sym v :> sub) = v -> MkRequest sub
    toRequest _ r v = toRequest (Proxy :: Proxy sub) (r { segments = segments r ++ [v'] })
        where
            v' = jsPack $ T.unpack $ toUrlPiece v

instance (KnownSymbol sym, ToHttpApiData a, HasAjaxRequest sub) => HasAjaxRequest (Header sym a :> sub) where
    type MkRequest (Header sym a :> sub) = Maybe a -> MkRequest sub
    toRequest _ r Nothing  = toRequest (Proxy :: Proxy sub) r
    toRequest _ r (Just a) = toRequest (Proxy :: Proxy sub) (r { rHeaders = rHeaders r ++ [(sym',a')]})
        where
            sym' = jsPack $ symbolVal (Proxy :: Proxy sym)
            a' = jsPack $ T.unpack $ toUrlPiece a

instance (KnownSymbol sym, HasAjaxRequest sub) => HasAjaxRequest (QueryFlag sym :> sub) where
    type MkRequest (QueryFlag sym :> sub) = Bool -> MkRequest sub
    toRequest _ r False = toRequest (Proxy :: Proxy sub) r
    toRequest _ r True = toRequest (Proxy :: Proxy sub) r { rQuery = rQuery r ++ [(sym', "true")]}
        where
            sym' = jsPack $ symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToHttpApiData a, HasAjaxRequest sub) => HasAjaxRequest (QueryParam sym a :> sub) where
    type MkRequest (QueryParam sym a :> sub) = Maybe a -> MkRequest sub
    toRequest _ r Nothing = toRequest (Proxy :: Proxy sub) r
    toRequest _ r (Just a) = toRequest (Proxy :: Proxy sub) r { rQuery = rQuery r ++ [(sym', a')]}
        where
            sym' = jsPack $ symbolVal (Proxy :: Proxy sym)
            a' = jsPack $ T.unpack $ toUrlPiece a

-- | Extracts 'Verb' body's content Needed
type family VBodyContent a where
  VBodyContent (Headers hdrs a) = a
  VBodyContent a = a

instance (ReflectMethod m, FromJSON (VBodyContent vbody))
    => HasAjaxRequest (Verb m s '[JSON] vbody) where
    type MkRequest (Verb m s '[JSON] vbody) = HandleResponse (VBodyContent vbody) -> IO ()
    toRequest _ r handler = do
        let query :: JSString = case rQuery r of
                        [] -> ""
                        qs -> "?" <> jsIntercalate "&" (map (\(x,y) -> x <> "=" <> y) qs)
        let req = AjaxRequest
                  { reqMethod = textToJSString $ T.decodeUtf8 $ reflectMethod (Proxy :: Proxy m)
                  , reqURI = jsIntercalate "/" (segments r) <> query
                  , reqHeaders = rHeaders r ++ [("Accept", "application/json")]
                  , reqBody = rBody r
                  , reqTimeout = rTimeout r
                  }
        ajax req $ \resp ->
            if respStatus resp < 300
                then do
                    case eitherDecode . cs . jsUnpack . respResponseText $ resp of
                        Left err -> handler $ Left (500, "Unable to convert response body: " <> err)  -- TODO: this is not an HTTP error.
                        Right v  -> handler $ Right v
                else handler $ Left (respStatus resp, jsUnpack $ respResponseText resp)
