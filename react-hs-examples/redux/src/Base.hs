module Base (Lens, Ref((:-:))) where

import React.Flux (SomeStoreAction)

-- Temporary and extremely evil
instance Eq (a -> b) where
    _ == _ = False

data Ref a = a :-: ((a -> IO a) -> SomeStoreAction)
    deriving Eq
infix 3 :-:

type Lens s a = (a -> IO a) -> s -> IO s
