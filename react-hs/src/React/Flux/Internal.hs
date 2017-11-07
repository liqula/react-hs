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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Internal module for React.Flux
--
-- Normally you should not need to use anything in this module.  This module is only needed if you have
-- complicated interaction with third-party javascript rendering code.
module React.Flux.Internal(
    ReactViewRef(..)
  , NewJsProps(..)
  , ReactElementRef(..)
  , ReactThis(..)
  , HandlerArg(..)
  , PropertyOrHandler_(..)
  , PropertyOrHandler
  , property
  , ReactElement_(..)
  , ReactElement
  , ReactElementM_(..)
  , ReactElementM
  , transHandler
  , elemString
  , elemText
  , elemJSString
  , elemShow
  , el
  , childrenPassedToView
  , elementToM
  , mkReactElement
  , exportViewToJs
  , exportNewViewToJs
  , toJSString
  , JSString
  , ($=)
  , (&=)
  , (@=)
  , classNames
  , classNamesLast
  , classNamesAny
  , unsafeDerefExport
  , fakeExport
  , fakeReExport
  , fakeJSValToExport
  , fakeDerefExport
  , StoreArg
  , StoreField
  , singleEq
  , allEq
  , AllEq(..)
  , UnoverlapAllEq
  , EventHandlerCode(..)
  , EventHandlerType
  , pushProp
) where

import           Control.Exception (throwIO, ErrorCall(ErrorCall))
import           Data.String (IsString(..))
import           Data.Aeson as A
import           Data.Maybe (maybe)
import qualified Data.HashMap.Strict as M
import           Data.Typeable
import           Control.Monad.Writer
import           Control.Monad.RWS
import           Control.Monad.Identity (Identity(..))
import qualified Data.Text as T
import           GHC.Generics

import           Unsafe.Coerce
import qualified Data.JSString as JSS
import qualified Data.JSString.Text as JSS
import           JavaScript.Array (JSArray)
import qualified JavaScript.Array as JSA
import           GHCJS.Foreign.Callback
import qualified JavaScript.Object as JSO
import           GHCJS.Types (JSVal, JSString, IsJSVal, jsval)
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Foreign (jsNull)
import           GHCJS.Foreign.Export


-- | This type is for the return value of @React.createClass@
newtype ReactViewRef (props :: k) = ReactViewRef { reactViewRef :: JSVal }
instance IsJSVal (ReactViewRef props)

-- | This type is for the return value of @React.createElement@
newtype ReactElementRef = ReactElementRef { reactElementRef :: JSVal }
instance IsJSVal ReactElementRef

-- | The first parameter of an event handler registered with React.
newtype HandlerArg = HandlerArg JSVal
  deriving (Generic)
instance IsJSVal HandlerArg

-- | The this value during the rendering function
newtype ReactThis state props = ReactThis {reactThisRef :: JSVal }
instance IsJSVal (ReactThis state props)

newtype NewJsProps = NewJsProps JSVal -- javascript list
instance IsJSVal NewJsProps

instance Show HandlerArg where
    show _ = "HandlerArg"

-- | Event handler type codes to make things less polymorphic
-- (we likely only ever need 'ViewEventHandler',
-- 'StatefulViewEventHandler', so just throwing around unconstrained type variables is a little
-- confusing).
-- The terminology ...Code is used in literature like in
-- http://www.larrytheliquid.com/drafts/leveling-up.pdf
-- section 2.1 Example of a Martin-Löf universe
data EventHandlerCode s = EventHandlerCode | StatefulEventHandlerCode s

-- | Meanings of event handler type codes
--
-- Instances are defined in React.Flux.Views.
--
-- (This could be a closed type family but avoiding circular module imports is non-trivial.  One
-- option would be to move this, PropertyOrHandler, but not PropertyOrHandler_, into
-- PropertiesAndEvents. This would require a few more imports in various places, importing Store
-- into PropertiesAndEvents and PropertiesAndEvents into DOM.
-- https://github.com/liqula/react-hs/pull/25#issuecomment-313048580)
type family EventHandlerType (x :: EventHandlerCode *) = r | r -> x

type PropertyOrHandler a = PropertyOrHandler_ (EventHandlerType a)

-- | Either a property or an event handler.
--
-- The combination of all properties and event handlers are used to create the javascript object
-- passed as the second argument to @React.createElement@.
data PropertyOrHandler_ handler =
   forall ref. ToJSVal ref => Property
      { propertyName :: JSString
      , propertyVal :: ref
      }
 | forall ref. ToJSVal ref => PropertyFromContext
      { propFromThisName :: JSString
      , propFromThisVal :: JSVal -> ref -- ^ will be passed this.context
      }
 | NestedProperty
      { nestedPropertyName :: JSString
      , nestedPropertyVals :: [PropertyOrHandler_ handler]
      }
 | ElementProperty
      { elementPropertyName :: JSString
      , elementValue :: ReactElementM_ handler ()
      }
 | CallbackPropertyWithArgumentArray
      { caPropertyName :: JSString
      , caFunc :: JSArray -> IO handler
      }
 | CallbackPropertyWithSingleArgument
      { csPropertyName :: JSString
      , csFunc :: HandlerArg -> IO handler
      }
 | forall props. Typeable props => CallbackPropertyReturningView
      { cretPropertyName :: JSString
      , cretArgToProp :: JSArray -> IO props
      , cretView :: ReactViewRef props
      }
  | CallbackPropertyReturningNewView
      { cretNewName :: JSString
      , cretNewView :: ReactViewRef ()
      , cretNewViewProps :: (JSArray -> IO NewJsProps)
      }

instance Functor PropertyOrHandler_ where
    fmap _ (Property name val) = Property name val
    fmap _ (PropertyFromContext name f) = PropertyFromContext name f
    fmap f (NestedProperty name vals) = NestedProperty name (map (fmap f) vals)
    fmap f (ElementProperty name (ReactElementM mkElem)) =
        ElementProperty name $ ReactElementM $ mapWriter (\((),e) -> ((), fmap f e)) mkElem
    fmap f (CallbackPropertyWithArgumentArray name h) = CallbackPropertyWithArgumentArray name (fmap f . h)
    fmap f (CallbackPropertyWithSingleArgument name h) = CallbackPropertyWithSingleArgument name (fmap f . h)
    fmap _ (CallbackPropertyReturningView name f v) = CallbackPropertyReturningView name f v
    fmap _ (CallbackPropertyReturningNewView name v p) = CallbackPropertyReturningNewView name v p

-- | Create a property from anything that can be converted to a JSVal
property :: (ToJSVal val) => JSString -> val -> PropertyOrHandler handler
property = Property

type ReactElement a = ReactElement_ (EventHandlerType a)

-- | A React element is a node or list of nodes in a virtual tree.  Elements are the output of the
-- rendering functions of classes.  React takes the output of the rendering function (which is a
-- tree of elements) and then reconciles it with the actual DOM elements in the browser.  The
-- 'ReactElement' is a monoid, so dispite its name can represent more than one element.  Multiple
-- elements are rendered into the browser DOM as siblings.
data ReactElement_ (eventHandler :: *)
    = ForeignElement
        { fName :: Either JSString (ReactViewRef Object)
        , fProps :: [PropertyOrHandler_ eventHandler]
        , fChild :: ReactElement_ eventHandler
        }
    | forall props. Typeable props => ViewElement
        { ceClass :: ReactViewRef props
        , ceKey :: Maybe JSVal
        , ceProps :: props
        , ceChild :: ReactElement_ eventHandler
        }
    | NewViewElement
        { newClass :: ReactViewRef ()
        , newViewKey :: Maybe JSString
        , newViewProps :: NewJsProps -> IO ()
        }
    | RawJsElement
        { rawTransform :: JSVal -> [ReactElementRef] -> IO ReactElementRef
        -- ^ first arg is this from render method, second argument is the rendering of 'rawChild'
        , rawChild :: ReactElement_ eventHandler
        }
    | ChildrenPassedToView
    | Content JSString
    | Append (ReactElement_ eventHandler) (ReactElement_ eventHandler)
    | EmptyElement

instance Monoid (ReactElement_ eventHandler) where
    mempty = EmptyElement
    mappend x y = Append x y

instance Functor ReactElement_ where
    fmap f (ForeignElement n p c) = ForeignElement n (map (fmap f) p) (fmap f c)
    fmap f (ViewElement n k p c) = ViewElement n k p (fmap f c)
    fmap _ (NewViewElement n k p) = NewViewElement n k p
    fmap f (RawJsElement t c) = RawJsElement t (fmap f c)
    fmap _ ChildrenPassedToView = ChildrenPassedToView
    fmap f (Append a b) = Append (fmap f a) (fmap f b)
    fmap _ (Content s) = Content s
    fmap _ EmptyElement = EmptyElement

-- | A writer monad for 'ReactElement's which is used in the rendering function of all views.
--
-- @do@ notation or the 'Monoid' instance is used to sequence sibling elements.
-- Child elements are specified via function application; the combinator creating an element takes
-- the child element as a parameter. The @OverloadedStrings@ extension is used to create plain text.
--
-- >ul_ $ do li_ (b_ "Hello")
-- >         li_ "World"
-- >         li_ $
-- >             ul_ (li_ "Nested" <> li_ "List")
--
-- would build something like
--
-- ><ul>
-- >  <li><b>Hello</b><li>
-- >  <li>World</li>
-- >  <li><ul>
-- >    <li>Nested</li>
-- >    <li>List</li>
-- >  </ul></li>
-- ></ul>
--
-- The "React.Flux.DOM" module contains a large number of combinators for creating HTML elements.
type ReactElementM e a = ReactElementM_ (EventHandlerType e) a

newtype ReactElementM_ eventHandler a = ReactElementM { runReactElementM :: Writer (ReactElement_ eventHandler) a }
    deriving (Functor, Applicative, Monad, Foldable)

-- | Create a 'ReactElementM' containing a given 'ReactElement'.
elementToM :: a -> ReactElement_ eventHandler -> ReactElementM_ eventHandler a
elementToM a e = ReactElementM (WriterT (Identity (a, e)))

instance (a ~ ()) => Monoid (ReactElementM_ eventHandler a) where
    mempty = elementToM () EmptyElement
    mappend e1 e2 =
        let ((),e1') = runWriter $ runReactElementM e1
            ((),e2') = runWriter $ runReactElementM e2
         in elementToM () $ Append e1' e2'

instance (a ~ ()) => IsString (ReactElementM_ eventHandler a) where
    fromString s = elementToM () $ Content $ toJSString s

-- | Transform the event handler for a 'ReactElementM'.
transHandler :: (handler1 -> handler2) -> ReactElementM_ handler1 a -> ReactElementM_ handler2 a
transHandler f (ReactElementM w) = ReactElementM $ mapWriter f' w
  where
    f' (a, x) = (a, fmap f x)

-- | Create a text element from a string. The text content is escaped to be HTML safe.
-- If you need to insert HTML, instead use the
-- <https://facebook.github.io/react/tips/dangerously-set-inner-html.html dangerouslySetInnerHTML>
-- property.  This is an alias for 'fromString'.
elemString :: String -> ReactElementM eventHandler ()
elemString s = elementToM () $ Content $ toJSString s

-- | Create a text element from a text value. The text content is escaped to be HTML safe.
elemText :: T.Text -> ReactElementM eventHandler ()
elemText s = elementToM () $ Content $ JSS.textToJSString s

-- | Create a text element from a @JSString@.  This is more efficient for hard-coded strings than
-- converting from text to a JavaScript string.  The string is escaped to be HTML safe.
elemJSString :: JSString -> ReactElementM eventHandler ()
elemJSString s = elementToM () $ Content s

-- | Create an element containing text which is the result of 'show'ing the argument.
-- Note that the resulting string is then escaped to be HTML safe.
elemShow :: (Show a) => a -> ReactElementM eventHandler ()
elemShow s = elementToM () $ Content $ toJSString $ show s

-- | Create a React element.
el :: JSString -- ^ The element name (the first argument to @React.createElement@).
   -> [PropertyOrHandler eventHandler] -- ^ The properties to pass to the element (the second argument to @React.createElement@).
   -> ReactElementM eventHandler a -- ^ The child elements (the third argument to @React.createElement@).
   -> ReactElementM eventHandler a
el name attrs (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ForeignElement (Left name) attrs childEl

-- | Transclude the children passed into 'React.Flux.view' or 'React.Flux.viewWithKey' into the
-- current rendering.  Use this where you would use @this.props.children@ in a javascript React
-- class.
childrenPassedToView :: ReactElementM eventHandler ()
childrenPassedToView = elementToM () ChildrenPassedToView

----------------------------------------------------------------------------------------------------
-- mkReactElement has two versions
----------------------------------------------------------------------------------------------------

type CallbackToRelease = JSVal

-- | Execute a ReactElementM to create a javascript React element and a list of callbacks attached
-- to nodes within the element.  These callbacks will need to be released with 'releaseCallback'
-- once the class is re-rendered.
mkReactElement :: forall eventHandler state props.
                  (EventHandlerType eventHandler -> IO ())
               -> ReactThis state props -- ^ this
               -> ReactElementM eventHandler ()
               -> IO (ReactElementRef, [CallbackToRelease])
mkReactElement runHandler this m = evalRWST (mToElem runHandler this m) () 0

-- Run the ReactElementM monad to create a ReactElementRef.
mToElem :: (EventHandlerType eventHandler -> IO ()) -> ReactThis state props -> ReactElementM eventHandler () -> MkReactElementM ReactElementRef
mToElem runHandler this eM = do
    let e = execWriter $ runReactElementM eM
        e' = case e of
                Content txt -> ForeignElement (Right $ ReactViewRef js_textWrapper) [] (Content txt)
                _ -> e
    refs <- createElement runHandler this e'
    case refs of
        [] -> lift $ js_ReactCreateElementNoChildren js_divLikeElement
        [x] -> return x
        xs -> lift $ do
            emptyObj <- JSO.create
            let arr = jsval $ JSA.fromList $ map reactElementRef xs
            js_ReactCreateForeignElement (ReactViewRef js_divLikeElement) emptyObj arr

-- add the property or handler to the javascript object
addPropOrHandlerToObj :: (EventHandlerType eventHandler -> IO ())
                      -> ReactThis state props
                      -> JSO.Object
                      -> PropertyOrHandler eventHandler
                      -> MkReactElementM ()
addPropOrHandlerToObj _ _ obj (Property n val) = lift $ do
    vRef <- toJSVal val
    JSO.setProp n vRef obj

addPropOrHandlerToObj _ this obj (PropertyFromContext n f) = lift $ do
    ctx <- js_ReactGetContext this
    vRef <- toJSVal $ f ctx
    JSO.setProp n vRef obj

addPropOrHandlerToObj runHandler this obj (NestedProperty n vals) = do
    nested <- lift $ JSO.create
    mapM_ (addPropOrHandlerToObj runHandler this nested) vals
    lift $ JSO.setProp n (jsval nested) obj

addPropOrHandlerToObj runHandler this obj (ElementProperty name rM) = do
    ReactElementRef ref <- mToElem runHandler this rM
    lift $ JSO.setProp name ref obj

addPropOrHandlerToObj runHandler _ obj (CallbackPropertyWithArgumentArray name func) = do
    -- this will be released by the render function of the class (jsbits/class.js)
    cb <- lift $ syncCallback1 ContinueAsync $ \argref -> do
        handler <- func $ unsafeCoerce argref
        runHandler handler
    tell [jsval cb]
    wrappedCb <- lift $ js_CreateArgumentsCallback cb
    lift $ JSO.setProp name wrappedCb obj

addPropOrHandlerToObj runHandler _ obj (CallbackPropertyWithSingleArgument name func) = do
    -- this will be released by the render function of the class (jsbits/class.js)
    cb <- lift $ syncCallback1 ContinueAsync $ \ref ->
        runHandler =<< func (HandlerArg ref)
    tell [jsval cb]
    lift $ JSO.setProp name (jsval cb) obj

addPropOrHandlerToObj _ _ obj (CallbackPropertyReturningView name toProps v) = do
    (cb, wrappedCb) <- lift $ exportViewToJs v toProps
    tell [cb]
    lift $ JSO.setProp name wrappedCb obj

addPropOrHandlerToObj _ _ obj (CallbackPropertyReturningNewView name v toProps) = do
    (cb, wrappedCb) <- lift $ exportNewViewToJs v toProps
    tell [cb]
    lift $ JSO.setProp name wrappedCb obj


type MkReactElementM a = RWST () [CallbackToRelease] Int IO a

generateKey :: MonadState Int m => m String
generateKey = do
  i <- state $ \i -> (i, i+1)
  pure $ "generated_key_" <> show i

-- | call React.createElement
createElement :: (EventHandlerType eventHandler -> IO ()) -> ReactThis state props -> ReactElement eventHandler -> MkReactElementM [ReactElementRef]
createElement _ _ EmptyElement = return []
createElement runHandler this (Append x y) = (++) <$> createElement runHandler this x <*> createElement runHandler this y
createElement _ _ (Content s) = return [js_ReactCreateContent s]

createElement _ this ChildrenPassedToView = lift $ do
  childRef <- js_ReactGetChildren this
  return $ map ReactElementRef $ JSA.toList childRef

createElement runHandler this (ForeignElement n p c) = do
    p' <- if null [k | Property k _ <- p, k == "key"]
         then do
           key <- generateKey
           pure $ p ++ [Property "key" key]
         else pure p
    let f = ForeignElement n p' c
    obj <- lift $ JSO.create
    mapM_ (addPropOrHandlerToObj runHandler this obj) $ fProps f
    childNodes <- createElement runHandler this $ fChild f
    let children = case map reactElementRef childNodes of
                     [] -> jsNull
                     [x] -> x
                     xs -> jsval $ JSA.fromList xs
    e <- lift $ case fName f of
        Left s -> js_ReactCreateElementName s obj children
        Right ref -> js_ReactCreateForeignElement ref obj children
    return [e]

createElement runHandler this (ViewElement { ceClass = rc, ceProps = props, ceKey = mkey, ceChild = child }) = do
    childNodes <- createElement runHandler this child
    propsE <- lift $ export props -- this will be released inside the lifetime events for the class (jsbits/class.js)
    let children = case map reactElementRef childNodes of
                     [] -> jsNull
                     [x] -> x
                     xs -> jsval $ JSA.fromList xs
    keyRef <- maybe (generateKey >>= lift . toJSVal . JSS.pack) pure mkey
    e <- lift $ js_ReactCreateKeyedElement rc keyRef propsE children
    return [e]

createElement _ _ (NewViewElement { newClass = rc, newViewKey = mk, newViewProps = buildProps}) = do
    k <- maybe (JSS.pack <$> generateKey) pure mk
    keyRef <- lift $ toJSVal k
    props <- lift $ js_emptyList
    lift $ buildProps props
    e <- lift $ js_ReactNewViewElement rc keyRef props
    return [e]

createElement runHandler this (RawJsElement trans child) = do
    childNodes <- createElement runHandler this child
    e <- liftIO $ trans (reactThisRef this) childNodes
    return [e]

js_ReactCreateContent :: JSString -> ReactElementRef
js_ReactCreateContent = ReactElementRef . unsafeCoerce

toJSString :: String -> JSString
toJSString = JSS.pack


exportViewToJs :: Typeable props => ReactViewRef props -> (JSArray -> IO props) -> IO (CallbackToRelease, JSVal)
exportViewToJs view toProps = do
    cb <- syncCallback2 ContinueAsync $ \ret argref -> do
        props <- toProps $ unsafeCoerce argref
        propsE <- export props -- this will be released inside the lifetime events for the class
        e <- js_ReactCreateClass view propsE jsNull
        js_setElemReturnFromCallback ret e
    wrappedCb <- js_wrapCallbackReturningElement cb
    return (jsval cb, wrappedCb)

exportNewViewToJs :: Typeable props => ReactViewRef props -> (JSArray -> IO NewJsProps) -> IO (CallbackToRelease, JSVal)
exportNewViewToJs view toProps = do
    cb <- syncCallback2 ContinueAsync $ \ret argref -> do
        props <- toProps $ unsafeCoerce argref
        e <- js_ReactNewViewElementNoKey view props
        js_setElemReturnFromCallback ret e
    wrappedCb <- js_wrapCallbackReturningElement cb
    return (jsval cb, wrappedCb)


----------------------------------------------------------------------------------------------------
--- Combinators
----------------------------------------------------------------------------------------------------

-- | Create a text-valued property.  This is here to avoid problems when OverloadedStrings extension
-- is enabled
($=) :: JSString -> JSString -> PropertyOrHandler handler
n $= a = Property n a
infixr 0 $=

-- | Create a property for anything that can be converted to a javascript value using the @ToJSVal@
-- class from the @ghcjs-base@ package..  This is just an infix version of 'property'.
(&=) :: ToJSVal a => JSString -> a -> PropertyOrHandler handler
n &= a = Property n a
infixr 0 &=

-- | Create a property from any aeson value (the at sign looks like "A" for aeson)
(@=) :: (A.ToJSON a) => JSString -> a -> PropertyOrHandler handler
n @= a = Property n (A.toJSON a)
infixr 0 @=

{-# DEPRECATED classNames "use classNamesLast" #-}
classNames :: [(T.Text, Bool)] -> PropertyOrHandler handler
classNames = classNamesLast

-- | Set the <https://facebook.github.io/react/docs/class-name-manipulation.html className> property to consist
-- of all the names which are matched with True, allowing you to easily toggle class names based on
-- a computation.
--
-- If a class is mentioned more than once, all but the *last* mentioning in the list are overruled.
-- See also: 'classNameAny'.
classNamesLast :: [(T.Text, Bool)] -> PropertyOrHandler handler
classNamesLast xs = "className" @= T.intercalate " " names
  where
    names = M.keys $ M.filter id $ M.fromList xs

-- | Variant of 'classNamesLast' that yields any class that has *any* flag set to 'True'.
classNamesAny :: [(T.Text, Bool)] -> PropertyOrHandler handler
classNamesAny xs = "className" @= T.intercalate " " names
  where
    names = M.keys $ M.fromList $ filter snd xs


-- TODO: we should not need any of the following.  (also, there are probably a few calls to
-- 'releaseExport' missing around this package.)

unsafeDerefExport :: Typeable a => String -> Export a -> IO a
unsafeDerefExport msg e = derefExport e
                      >>= maybe (throwIO (ErrorCall $ "unsafeDerefExport: " <> show (typeOf e, msg))) pure

fakeExport :: Typeable a => a -> IO (Export a)
fakeExport = pure . unsafeCoerce  -- should be the same as @pure . Export@

fakeReExport :: Typeable a => JSVal -> IO (Export a)
fakeReExport = export . unsafeCoerce

fakeJSValToExport :: Typeable a => JSVal -> Export a
fakeJSValToExport = unsafeCoerce

fakeDerefExport :: Typeable a => Export a -> IO a
fakeDerefExport = pure . unsafeCoerce

pushProp :: Typeable a => a -> NewJsProps -> IO ()
pushProp val props = do
  valE <- export $! val -- this will be released in the lifecycle callbacks of the class
  js_pushProp props valE


-- | If you want to pass a store value into a component via 'mkControllerView', make entry in the
-- type list you apply have type @StoreArg <store>@.  See also: 'StoreField'; see test client for an
-- example how to use this.
data StoreArg store

-- | If you want to pass a *part of a* store value into a component via 'mkControllerView', make entry in the
-- type list you apply have type @StoreField <store> "<fieldname>" <fieldtype>@ (field name is a
-- type-level string literal).  See also: 'StoreArg'; see test client for an example how to use
-- this.
data StoreField store (fieldname :: k) fieldtype

type ForeignEq = JSVal -> JSVal -> IO JSVal
type ForeignEq_ = JSVal -> JSVal -> IO Bool

singleEq :: forall (t :: *). (Typeable t, Eq t) => Proxy t -> IO (Callback ForeignEq)
singleEq proxy = syncCallback2' (\jsa jsb -> toJSVal =<< singleEq_ proxy jsa jsb)

singleEq_ :: forall (t :: *). (Typeable t, Eq t) => Proxy t -> ForeignEq_
singleEq_ Proxy (fakeJSValToExport -> (jsa :: Export t)) (fakeJSValToExport -> (jsb :: Export t)) = do
  a :: t <- unsafeDerefExport "singleEq_.a" jsa
  b :: t <- unsafeDerefExport "singleEq_.b" jsb
  pure $ a == b

allEq :: AllEq t => Proxy t -> IO (Callback ForeignEq)
allEq proxy = syncCallback2' (\jsa jsb -> toJSVal =<< allEq_ proxy 0 jsa jsb)

type family StoreType a where
  StoreType (StoreArg st) = st
  StoreType (StoreField st fn ft) = ft
  StoreType a = a

class AllEq t where
  allEq_ :: Proxy t -> Int -> ForeignEq_

instance AllEq '[] where
  allEq_ Proxy _ _ _ = pure True

instance (Typeable (StoreType t), Eq (StoreType t), AllEq ts)
      => AllEq (t ': ts) where
  allEq_ Proxy i jsas jsbs = do
    let jsa = js_findFromArray i jsas
        jsb = js_findFromArray i jsbs
    (&&) <$> singleEq_ (Proxy :: Proxy (StoreType t)) jsa jsb
         <*> allEq_    (Proxy :: Proxy ts) (i + 1) jsas jsbs


{-# DEPRECATED UnoverlapAllEq "not required any more, just remove the instance." #-}
class UnoverlapAllEq t

#ifdef __GHCJS__

foreign import javascript unsafe
    "React['createElement']($1)"
    js_ReactCreateElementNoChildren :: JSVal -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, $2, $3)"
    js_ReactCreateElementName :: JSString -> JSO.Object -> JSVal -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, $2, $3)"
    js_ReactCreateForeignElement :: ReactViewRef a -> JSO.Object -> JSVal -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, {hs:$2}, $3)"
    js_ReactCreateClass :: ReactViewRef a -> Export props -> JSVal -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, {key: $2, hs:$3}, $4)"
    js_ReactCreateKeyedElement :: ReactViewRef a -> JSVal -> Export props -> JSVal -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, {key: $2, hs:$3})"
    js_ReactNewViewElement :: ReactViewRef a -> JSVal -> NewJsProps -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, {hs:$2})"
    js_ReactNewViewElementNoKey :: ReactViewRef a -> NewJsProps -> IO ReactElementRef

foreign import javascript unsafe
    "hsreact$mk_arguments_callback($1)"
    js_CreateArgumentsCallback :: Callback (JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe
    "hsreact$wrap_callback_returning_element($1)"
    js_wrapCallbackReturningElement :: Callback (JSVal -> JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe
    "$1.elem = $2"
    js_setElemReturnFromCallback :: JSVal -> ReactElementRef -> IO ()

foreign import javascript unsafe
    "$r = hsreact$divLikeElement"
    js_divLikeElement :: JSVal

foreign import javascript unsafe
    "$r = hsreact$textWrapper"
    js_textWrapper :: JSVal

foreign import javascript unsafe
    "$1['context']"
    js_ReactGetContext :: ReactThis state props -> IO JSVal

foreign import javascript unsafe
    "hsreact$children_to_array($1['props']['children'])"
    js_ReactGetChildren :: ReactThis state props -> IO JSArray

foreign import javascript unsafe
    "[]"
    js_emptyList :: IO NewJsProps

-- | (This is similar to findFromState, but less specific, and more "pure".  not sure if we can merge
-- the two?)
foreign import javascript unsafe
  "$2[$1]"
  js_findFromArray :: Int -> JSVal -> JSVal

foreign import javascript unsafe
  "$1.push($2)"
  js_pushProp :: NewJsProps -> Export a -> IO ()

#else

js_ReactCreateElementNoChildren :: JSVal -> IO ReactElementRef
js_ReactCreateElementNoChildren _ = error "js_ReactCreateElementNoChildren only works with GHCJS"

js_ReactCreateElementName :: JSString -> JSO.Object -> JSVal -> IO ReactElementRef
js_ReactCreateElementName _ _ _ = error "js_ReactCreateElementName only works with GHCJS"

js_ReactCreateForeignElement :: ReactViewRef a -> JSO.Object -> JSVal -> IO ReactElementRef
js_ReactCreateForeignElement _ _ _ = error "js_ReactCreateForeignElement only works with GHCJS"

js_ReactCreateClass :: ReactViewRef a -> Export props -> JSVal -> IO ReactElementRef
js_ReactCreateClass _ _ _ = error "js_ReactCreateClass only works with GHCJS"

js_ReactCreateKeyedElement :: ReactViewRef a -> JSVal -> Export props -> JSVal -> IO ReactElementRef
js_ReactCreateKeyedElement _ _ _ _ = error "js_ReactCreateKeyedElement only works with GHCJS"

js_ReactNewViewElement :: ReactViewRef a -> JSVal -> NewJsProps -> IO ReactElementRef
js_ReactNewViewElement _ _ _ = error "js_ReactNewViewElement only works with GHCJS"

js_ReactNewViewElementNoKey :: ReactViewRef a -> NewJsProps -> IO ReactElementRef
js_ReactNewViewElementNoKey _ _ = error "js_ReactNewViewElementNoKey only works with GHCJS"

js_CreateArgumentsCallback :: Callback (JSVal -> IO ()) -> IO JSVal
js_CreateArgumentsCallback _ = error "js_CreateArgumentsCallback only works with GHCJS"

js_wrapCallbackReturningElement :: Callback (JSVal -> JSVal -> IO ()) -> IO JSVal
js_wrapCallbackReturningElement _ = error "js_wrapCallbackReturningElement only works with GHCJS"

js_setElemReturnFromCallback :: JSVal -> ReactElementRef -> IO ()
js_setElemReturnFromCallback _ _ = error "js_setElemReturnFromCallback only works with GHCJS"

js_divLikeElement :: JSVal
js_divLikeElement = error "js_divLikeElement only works with GHCJS"

js_textWrapper :: JSVal
js_textWrapper = error "js_textWrapper only works with GHCJS"

js_ReactGetContext :: ReactThis state props -> IO JSVal
js_ReactGetContext _ = error "js_ReactGetContext only works with GHCJS"

js_ReactGetChildren :: ReactThis state props -> IO JSArray
js_ReactGetChildren _ = error "js_ReactGetChildren only works with GHCJS"

js_emptyList :: IO NewJsProps
js_emptyList = error "js_emptyList only works with GHCJS"

js_findFromArray :: Int -> JSVal -> JSVal
js_findFromArray _ _ = error "js_findFromArray only works with GHCJS"

js_pushProp :: NewJsProps -> Export a -> IO ()
js_pushProp _ _ = error "js_pushProp only works with GHCJS"

#endif
