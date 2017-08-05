module State.Left (LeftState(LeftState)) where

data LeftState = LeftState String [String]
    deriving Eq
