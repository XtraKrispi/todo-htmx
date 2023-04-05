{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (traverse_)
import Data.Text (Text, pack, unwords)
import Data.Text.Lazy qualified as LT
import Database.SQLite.Simple (Only (Only), execute, execute_, query, query_, withConnection)
import Htmx (hxDelete, hxGet, hxPatch, hxPost, hxSwap, hxSwapOob, hxTarget, hxTrigger)
import Htmx.ResponseHeaders (hxTriggerResponse)
import Lucid (Attribute, Html, ToHtml (toHtml), renderText)
import Lucid.Base (makeAttribute)
import Lucid.Html5
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (static)
import Todo (Todo (..))
import Web.Scotty (Parsable (parseParam), delete, get, html, middleware, param, params, patch, post, scotty)
import Web.Scotty.Internal.Types (ActionT)

tshow :: Show a => a -> Text
tshow = pack . show

applyAttribute :: Bool -> Attribute -> Attribute
applyAttribute condition attr = if condition then attr else makeAttribute "" ""

getOptionalParameter :: Parsable a => LT.Text -> ActionT LT.Text IO (Maybe a)
getOptionalParameter paramName = do
  allParams <- params
  case filter (\(p, _) -> p == paramName) allParams of
    [(_, pVal)] -> do
      let v = parseParam pVal
      case v of
        Left _ -> pure Nothing
        Right v' -> pure (Just v')
    _ -> pure Nothing

data Filter
  = All
  | Active
  | Completed
  deriving (Eq)

instance Parsable Filter where
  parseParam :: LT.Text -> Either LT.Text Filter
  parseParam "active" = pure Active
  parseParam "completed" = pure Completed
  parseParam "all" = pure All
  parseParam _ = Left "invalid filter"

indexPage :: Html ()
indexPage = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "HTMx Test!"
      link_
        [ rel_ "stylesheet"
        , href_ "/static/base.css"
        , type_ "text/css"
        ]
      link_
        [ rel_ "stylesheet"
        , href_ "/static/index.css"
        , type_ "text/css"
        ]

    body_ [] $ do
      section_ [class_ "todoapp"] do
        header_ [class_ "header"] do
          h1_ "todos"
          newTodoInput
        todosSection True []
        footer All []
      footer_ [class_ "info"] do
        p_ "Double-click to edit a todo"
        p_ do
          "Created by "
          a_ [href_ "https://twitter.com/oscargodson"] "Oscar Godson"
        p_ do
          "Refactored by "
          a_ [href_ "https://github.com/cburgmer"] "Christoph Burgmer"
        p_ do
          "Part of "
          a_ [href_ "http://todomvc.com/"] "TodoMVC"
      script_ [src_ "https://unpkg.com/htmx.org@1.8.6"] (mempty @Text)

renderTodo :: Bool -> Todo -> Html ()
renderTodo isEdit todo =
  let liClass = Data.Text.unwords $ fst <$> filter snd [("completed", todo.isCompleted), ("editing", isEdit)]
   in li_ [class_ liClass] do
        div_ [class_ "view"] do
          input_
            [ class_ "toggle"
            , type_ "checkbox"
            , name_ "is-completed"
            , hxPatch ("/api/todos/" <> tshow todo.id <> "/is-completed")
            , applyAttribute todo.isCompleted checked_
            ]
          label_ [hxGet ("/api/todos/" <> tshow todo.id), hxTrigger "dblclick"] (toHtml todo.description)
          button_ [class_ "destroy", hxDelete ("/api/todos/" <> tshow todo.id)] ""
        when isEdit do
          input_
            [ value_ todo.description
            , class_ "edit"
            , name_ "description"
            , hxTrigger "blur"
            , hxPatch ("/api/todos/" <> tshow todo.id <> "/description")
            ]
todosSection :: Bool -> [Todo] -> Html ()
todosSection onLoad todos =
  if null todos
    then
      section_
        [ class_ "main"
        , style_ "display: none"
        , hxGet "/api/todos"
        , hxTrigger (if onLoad then "load" else "refresh-items from:body")
        , hxSwap "outerHTML"
        ]
        ""
    else section_
      [ class_ "main"
      , style_ "display: block"
      , hxGet "/api/todos"
      , hxTrigger (if onLoad then "load" else "refresh-items from:body")
      , hxSwap "outerHTML"
      ]
      do
        toggleAll (all (.isCompleted) todos)
        ul_ [class_ "todo-list"] (traverse_ (renderTodo False) todos)

footer :: Filter -> [Todo] -> Html ()
footer currFilter todos =
  if null todos
    then
      footer_
        [ class_ "footer"
        , id_ "footer"
        , style_ "display: none"
        , hxSwapOob
        ]
        ""
    else footer_
      [ class_ "footer"
      , style_ "display: block"
      , id_ "footer"
      , hxSwapOob
      ]
      do
        span_ [class_ "todo-count"] do
          let numIncomplete = length $ filter (not . (.isCompleted)) todos
          strong_ (toHtml $ tshow numIncomplete)
          " item" <> (if numIncomplete == 1 then "" else "s") <> " left"
        ul_ [class_ "filters"] do
          li_ do
            a_
              [ hxGet "/api/todos/filter/all"
              , hxTarget ".main"
              , applyAttribute (currFilter == All) (class_ "selected")
              , href_ ""
              ]
              "All"
          li_ do
            a_
              [ hxGet "/api/todos/filter/active"
              , hxTarget ".main"
              , applyAttribute (currFilter == Active) (class_ "selected")
              , href_ ""
              ]
              "Active"
          li_ do
            a_
              [ hxGet "/api/todos/filter/completed"
              , hxTarget ".main"
              , applyAttribute (currFilter == Completed) (class_ "selected")
              , href_ ""
              ]
              "Completed"
        when (any ((.isCompleted)) todos) do
          button_ [class_ "clear-completed", hxDelete "/api/todos/clear-completed"] "Clear completed"
toggleAll :: Bool -> Html ()
toggleAll checked = do
  input_
    [ id_ "toggle-all"
    , name_ "toggle-all"
    , class_ "toggle-all"
    , type_ "checkbox"
    , applyAttribute checked checked_
    , hxPost "/api/todos/all"
    , hxSwap "outerHTML"
    ]
  label_ [for_ "toggle-all"] ""

newTodoInput :: Html ()
newTodoInput =
  input_
    [ class_ "new-todo"
    , name_ "new-todo"
    , placeholder_ "What needs to be done?"
    , hxPost "/api/todos"
    , hxTrigger "keyup[key=='Enter']"
    , hxSwap "outerHTML"
    ]

processTodo :: Filter -> ActionT LT.Text IO ()
processTodo filterVal = do
  allTodos <- liftIO $ withConnection dbName (`query_` "SELECT * FROM todos")
  html $
    renderText do
      footer filterVal allTodos
      todosSection
        False
        ( filter
            ( \t -> case filterVal of
                Active -> not t.isCompleted
                Completed -> t.isCompleted
                All -> True
            )
            allTodos
        )

dbName :: String
dbName = "test.db"

main :: IO ()
main = do
  scotty 8080 do
    middleware logStdoutDev
    middleware static
    get "/" $ html $ renderText indexPage
    get "/api/todos" $ processTodo All
    post "/api/todos" do
      newTodo <- param "new-todo"
      liftIO $
        withConnection
          dbName
          ( \conn -> execute conn "INSERT INTO todos(description, is_completed) VALUES(?, ?)" (newTodo :: Text, False)
          )
      hxTriggerResponse "refresh-items"
      html $
        renderText newTodoInput
    delete "/api/todos/clear-completed" do
      liftIO $
        withConnection
          dbName
          (`execute_` "DELETE FROM todos WHERE is_completed = 1")
      hxTriggerResponse "refresh-items"
      html ""

    delete "/api/todos/:id" do
      todoId :: Int <- param "id"
      liftIO $
        withConnection
          dbName
          ( \conn -> execute conn "DELETE FROM todos WHERE Id = ?" [todoId]
          )
      hxTriggerResponse "refresh-items"
      html ""
    patch "/api/todos/:id/is-completed" do
      todoId :: Int <- param "id"
      isCompleted <- maybe False (\(c :: Text) -> c == "on") <$> getOptionalParameter "is-completed"
      liftIO $
        withConnection
          dbName
          ( \conn -> execute conn "UPDATE todos SET is_completed = ? WHERE Id = ?" (isCompleted, todoId)
          )
      hxTriggerResponse "refresh-items"
      html ""
    post "/api/todos/all" do
      checked <- maybe False (\(c :: Text) -> c == "on") <$> getOptionalParameter "toggle-all"
      liftIO $
        withConnection
          dbName
          ( \conn -> execute conn "UPDATE todos SET is_completed = ?" [checked]
          )
      hxTriggerResponse "refresh-items"
      html $ renderText $ toggleAll checked
    get "/api/todos/filter/:filter" do
      filterParam :: Filter <- param "filter"
      processTodo filterParam
    get "/api/todos/:id" do
      todoId :: Int <- param "id"
      [todo] <- liftIO $ withConnection dbName (\conn -> query conn "SELECT * FROM todos WHERE id = ?" (Only todoId))
      html $ renderText $ renderTodo True todo
    patch "/api/todos/:id/description" do
      todoId :: Int <- param "id"
      description :: Text <- param "description"
      when (description /= "") $
        liftIO $
          withConnection
            dbName
            ( \conn -> execute conn "UPDATE todos SET description = ? WHERE Id = ?" (description, todoId)
            )
      hxTriggerResponse "refresh-items"
      html ""
