{-# LANGUAGE OverloadedStrings, BangPatterns, DataKinds, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | The views for the TODO app
module TodoViews where

import Control.Monad (when)
import GHCJS.Types
import React.Flux
import React.Flux.Outdated
import qualified Data.JSString as JSS

import TodoDispatcher
import TodoStore
import TodoComponents

-- | The controller view and also the top level of the TODO app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
todoApp :: View '[]
todoApp = mkControllerView @'[StoreArg TodoState] "todo app" $ \todoState ->
    div_ $ do
        view_ todoHeader "header"
        mainSection_ todoState
        view_ todoFooter "footer" todoState
        testComponentDidMount_ $ do
          div_ $ do
            span_ $ do
              "mempty"
            span_ $ do
              "mempty"
            span_ $ do
              "mempty"

-- | The TODO header as a React view with no properties.
todoHeader :: View '[]
todoHeader = mkView "header" $
    header_ ["id" $= "header"] $ do
        h1_ "todos"
        todoTextInput_  TextInputArgs
          { tiaId = Just "new-todo"
          , tiaClass = "new-todo"
          , tiaPlaceholder = "What needs to be done?"
          , tiaSaveAction = SACreate
          , tiaValue = Nothing
          }

-- | A view that does not use a ReactView and is instead just a Haskell function.
-- Note how we use an underscore to signal that this is directly a combinator that can be used
-- inside the rendering function.
mainSection_ :: TodoState -> ReactElementM ViewEventHandler ()
mainSection_ st = section_ ["id" $= "main"] $ do
    labeledInput_ "toggle-all" "Mark all as complete"
        [ "type" $= "checkbox"
        , "checked" $= if all (todoComplete . snd) $ todoList st then "checked" else ""
        , onChange $ \_ -> dispatchTodo ToggleAllComplete
        ]

    ul_ [ "id" $= "todo-list" ] $ mapM_ todoItem_ $ todoList st

instance UnoverlapAllEq Int
instance UnoverlapAllEq Todo

-- | A view for each todo item.  We specifically use a ReactView here to take advantage of the
-- ability for React to only re-render the todo items that have changed.  Care is taken in the
-- transform function of the store to not change the Haskell object for the pair (Int, Todo), and
-- in this case React will not re-render the todo item.  For more details, see the "Performance"
-- section of the React.Flux documentation.
todoItem :: View '[Int, Todo]
todoItem = mkView "todo item" $ \todoIdx todo ->
    li_ [ classNamesLast [("completed", todoComplete todo), ("editing", todoIsEditing todo)]
        , "key" @= todoIdx
        ] $ do

        cldiv_ "view" $ do
            input_ [ "className" $= "toggle"
                   , "type" $= "checkbox"
                   , "checked" @= todoComplete todo
                   , onChange $ \_ -> dispatchTodo $ TodoSetComplete todoIdx $ not $ todoComplete todo
                   ]

            label_ [ onDoubleClick $ \_ _ -> dispatchTodo $ TodoEdit todoIdx] $
                elemText $ todoText todo

            clbutton_ "destroy" (dispatchTodo $ TodoDelete todoIdx) mempty

        when (todoIsEditing todo) $
            todoTextInput_ TextInputArgs
                { tiaId = Nothing
                , tiaClass = "edit"
                , tiaPlaceholder = ""
                , tiaSaveAction = SAUpdate todoIdx
                , tiaValue = Just $ todoText todo
                }

-- | A combinator for a todo item to use inside rendering functions
todoItem_ :: (Int, Todo) -> ReactElementM eventHandler ()
todoItem_ (i, t) = view_ todoItem (JSS.pack $ show i) i t

instance UnoverlapAllEq TodoState

-- | A view for the footer, taking the entire state as the properties.  This could alternatively
-- been modeled as a controller-view, attaching directly to the store.
todoFooter :: View '[TodoState]
todoFooter = mkView "footer" $ \(TodoState todos) ->
    let completed = length (filter (todoComplete . snd) todos)
        itemsLeft = length todos - completed
     in footer_ [ "id" $= "footer"] $ do

            span_ [ "id" $= "todo-count" ] $ do
                strong_ $ elemShow itemsLeft
                elemText $ if itemsLeft == 1 then " item left" else " items left"

            when (completed > 0) $ do
                button_ [ "id" $= "clear-completed"
                        , onClick $ \_ _ -> dispatchTodo ClearCompletedTodos
                        ] $
                    elemString $ "Clear completed (" ++ show completed ++ ")"



testComponentDidMount :: ReactView ()
testComponentDidMount = defineLifecycleView "testComponentDidMount" () lifecycleConfig
{-
  { lRender = \_state props -> do
      let dataContributionId = props ^. markPropsContributionID
          statefulEventHandlers :: [PropertyOrHandler (() -> ([SomeStoreAction], Maybe ()))]
          statefulEventHandlers = (\h _ -> (h, Nothing)) <$$> (attrToProp <$> props ^. markPropsAttrs)
      mark_ (statefulEventHandlers <>
           [ classNamesAny
                        [ ("o-mark", True)
                        , ("o-mark--highlight", Just dataContributionId == props ^. markPropsDisplayedContribution)
                        , ("o-mark--hover",     Just dataContributionId == props ^. markPropsHighlightedMark)
                        , (cs $ "o-mark--" <> contributionIDToKindST dataContributionId,
                                                Just dataContributionId /= props ^. markPropsDisplayedContribution)
                        ]
           , onMouseEnter $ \_ _ _ -> (dispatch . ContributionAction $ HighlightMarkAndBubble dataContributionId, Nothing)
           , onMouseLeave $ \_ _ _ -> (dispatch $ ContributionAction UnhighlightMarkAndBubble, Nothing)
           , "ref" &= unsafePerformIO (dispatchAddMarkPosition dataContributionId)
           ]) childrenPassedToView

   , lComponentDidMount = Just $ \propsandstate ldom _ -> do
             props  <- lGetProps propsandstate
             mark   <- lThis ldom
             dispatchAddMarkPosition (props ^. markPropsContributionID) mark
   }
-}
  { lRender = \_state _props -> do
      mark_ [] $ do
        "<<<testComponentDidMount>>>"
        childrenPassedToView

  , lComponentDidMount = Just $ \_propsandstate _ldom _ -> do
      executeAction `mapM_` dispatchTodo (TodoCreate "lba")
      js_alert "<<<testComponentDidMount!!>>>"
  }

testComponentDidMount_ :: ReactElementM eventHandler () -> ReactElementM eventHandler ()
testComponentDidMount_ = view testComponentDidMount ()


foreign import javascript unsafe
  "window.alert($1)"
  js_alert :: JSString -> IO ()
