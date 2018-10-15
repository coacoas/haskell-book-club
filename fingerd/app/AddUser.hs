{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Data.Char
import Data.Text
import qualified Database.SQLite.Simple as SQLite
import Haskellbook.Fingerd.Db

parseInteger :: Text -> Integer
parseInteger t = foldl' go (0 :: Integer) t
  where go :: Integer -> Char -> Integer
        go acc ch = (10 * acc) + (Prelude.toInteger $ digitToInt ch)

prompt :: String -> IO Text
prompt msg = fmap pack $ putStr msg >> getLine

main :: IO ()
main = do
  _name     <- prompt "Username: "
  _shell    <- prompt "Shell: "
  _dir      <- prompt "Home directory: "
  _realName <- prompt "User's real name: "
  _phone    <- prompt "Phone number: "

  users     <-  bracket
    (SQLite.open "finger.db")
    (SQLite.close)
    (\conn -> do
      addUser conn _name _shell _dir _realName _phone
      getAllUsers conn)

  print users
