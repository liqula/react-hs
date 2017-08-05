{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}



module Main where

import React.Flux
import React.Flux.Outdated


foreign import javascript unsafe "loadImage();"
  loadImage :: IO ()


data AppState = A | B deriving (Eq, Show)

initStore :: AppState
initStore = A

data Action = ChangeAB
    
dispatch :: Action -> [SomeStoreAction]
dispatch a = [action @AppState a]

sDispatch :: Action -> ([SomeStoreAction], [EventModification])
sDispatch = simpleHandler . dispatch

instance StoreData AppState where
  type StoreAction AppState = Action

  transform ChangeAB A = return B
  transform ChangeAB B = return A

lcConfig :: LifecycleViewConfig props state
lcConfig = lifecycleConfig {
  lComponentDidMount = Just (\_ _ _ -> loadImage)
  , lComponentWillUnmount = Just (\_ _ -> putStrLn "unmount")
  }

myImage :: ReactView ()
myImage = defineLifecycleView "image-view" () lcConfig


myImage_ :: ReactElementM_ [SomeStoreAction] ()
myImage_ = view myImage () mempty

renderA :: ReactElementM_ [SomeStoreAction] ()
renderA = div_ [] $ do
  canvas_ [ "id" $= "my-canvas" ] mempty
  myImage_

  
renderB :: ReactElementM_ [SomeStoreAction] ()
renderB = div_ [] (elemString "B")


render :: AppState -> ReactElementM_ [SomeStoreAction] ()
render state = do
  a_ [ "href" $= "#", onClick $ \_ _ -> sDispatch ChangeAB  ] (elemString "Click to change view")
  case state of
   A -> renderA
   B -> renderB

   
app :: View '[]
app = mkControllerView @'[StoreArg AppState] "app" render

main :: IO ()
main = do
  registerInitialStore initStore
  reactRenderView "app" app
