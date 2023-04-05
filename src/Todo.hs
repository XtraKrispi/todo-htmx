module Todo where

import Data.Text (Text)
import Database.SQLite.Simple (FromRow (fromRow), field)
import Database.SQLite.Simple.FromRow (RowParser)

data Todo = Todo
  { id :: Int
  , description :: Text
  , isCompleted :: Bool
  }
  deriving (Show)

instance FromRow Todo where
  fromRow :: RowParser Todo
  fromRow = Todo <$> field <*> field <*> field