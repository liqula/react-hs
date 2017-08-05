module State (State(State)) where

import State.Left

data State = State LeftState Int
    deriving Eq
