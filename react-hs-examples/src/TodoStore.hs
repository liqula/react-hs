{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
module TodoStore where

import React.Flux
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import qualified Data.Text as T

data Todo = Todo {
    todoText :: T.Text
  , todoComplete :: Bool
  , todoIsEditing :: Bool
} deriving (Eq, Show, Typeable)

newtype TodoState = TodoState {
    todoList :: [(Int, Todo)]
} deriving (Eq, Show, Typeable)

data TodoAction = TodoCreate T.Text
                | TodoDelete Int
                | TodoEdit Int
                | UpdateText Int T.Text
                | ToggleAllComplete
                | TodoSetComplete Int Bool
                | ClearCompletedTodos
  deriving (Show, Typeable, Generic)

instance StoreData TodoState where
    type StoreAction TodoState = TodoAction
    transform a (TodoState todos) = do
        putStrLn $ "Action: " ++ show a
        putStrLn $ "Initial todos: " ++ show todos

        -- Care is taken here to leave the Haskell object for the pair (Int, Todo) unchanged if the todo
        -- itself is unchanged.  This allows React to avoid re-rendering the todo when it does not change.
        -- For more, see the "Performance" section of the React.Flux haddocks.
        newTodos <- return $ case a of
            (TodoCreate txt) -> (maximum (map fst todos) + 1, Todo txt False False) : todos
            (TodoDelete i) -> filter ((/=i) . fst) todos
            (TodoEdit i) -> let f (idx, todo) | idx == i = (idx, todo { todoIsEditing = True })
                                f p = p
                             in map f todos
            (UpdateText newIdx newTxt) ->
                let f (idx, todo) | idx == newIdx = (idx, todo { todoText = newTxt, todoIsEditing = False })
                    f p = p
                 in map f todos
            ToggleAllComplete -> [ (idx, Todo txt True False) | (idx, Todo txt _ _) <- todos ]
            TodoSetComplete newIdx newComplete ->
                let f (idx, todo) | idx == newIdx = (idx, todo { todoComplete = newComplete })
                    f p = p
                 in map f todos
            ClearCompletedTodos -> filter (not . todoComplete . snd) todos

        putStrLn $ "New todos: " ++ show newTodos
        return $ TodoState newTodos

initialTodos :: TodoState
initialTodos = TodoState
    [ (0, Todo "Learn react" True False)
    , (1, Todo "Learn react-hs" False False)
    ]
