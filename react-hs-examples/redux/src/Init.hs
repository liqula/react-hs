module Init (getInitial) where

import State
import State.Left

getInitial :: IO State
getInitial = pure $ State (LeftState "" ["foo", "bar", "baz"]) 0
