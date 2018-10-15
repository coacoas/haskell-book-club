{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskellbook.Fingerd.Db( createDatabase
         , getAllUsers
         , getUser
         , addUser
         , User(..)
         ) where

import Control.Exception
import Data.Text (Text)
import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Text.RawString.QQ

data User =
  User { userId        :: Integer
       , username      :: Text
       , shell         :: Text
       , homeDirectory :: Text
       , realName      :: Text
       , phone         :: Text
       } deriving (Eq, Show)

instance FromRow User where
  fromRow = User
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

instance ToRow User where
  toRow (User id_ username_ shell_ homeDir_ realName_ phone_) =
    toRow (id_, username_, shell_, homeDir_, realName_, phone_)

createUsers :: Query
createUsers = [r|
  CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE,
  shell TEXT,
  homeDirectory TEXT,
  realName TEXT,
  phone TEXT) |]

insertUser :: Query
insertUser =
  "INSERT INTO users VALUES(?,?,?,?,?,?)"

allUsers :: Query
allUsers =
  "SELECT * from users"

getUserQuery :: Query
getUserQuery =
  "SELECT * from users where username = ?"

data DuplicateData = DuplicateData
  deriving (Eq, Show, Typeable, Exception)

type UserRow = (Null, Text, Text, Text, Text, Text)

getAllUsers :: Connection
         -> IO [User]
getAllUsers conn = query_ conn allUsers

getUser :: Connection
        -> Text
        -> IO (Maybe User)
getUser conn username_ = do
  results <- query conn getUserQuery (Only username_)
  case results of
    []     -> return $ Nothing
    [user] -> return $ Just user
    _      -> throwIO DuplicateData

addUser :: Connection
           -> Text
           -> Text
           -> Text
           -> Text
           -> Text
           -> IO ()
addUser conn _id _shell hd fn ph = execute conn insertUser $
  (Null, _id, _shell, hd, fn, ph)

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  execute conn insertUser callenRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where callenRow :: UserRow
        callenRow = (Null
                    , "callen"
                    , "/bin/zsh"
                    , "/home/callen"
                    , "Chris Allen"
                    , "555-123-4567")
        meRow :: UserRow
        meRow = (Null
                , "bill.carlson"
                , "/bin/bash"
                , "/home/bill.carlson"
                , "Bill Carlson"
                , "904-867-5309")
