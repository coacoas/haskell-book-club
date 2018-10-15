{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.SQLite.Simple as SQLite
import Network.Socket hiding (recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)

import Haskellbook.Fingerd.Db


returnUsers :: SQLite.Connection
            -> Socket
            ->IO()
returnUsers dbConn soc = do
  rows <- getAllUsers dbConn
  let usernames = map username rows
      newlineSeparated = T.concat $
                         intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

returnUser :: SQLite.Connection
           -> Socket
           -> Text
           -> IO()
returnUser dbConn soc username_ = do
  maybeUser <- getUser dbConn (T.strip username_)
  case maybeUser of
    Nothing -> do
      let msg :: ByteString
          msg = BS.concat ["Couldn't find matching user for username: "
                          , encodeUtf8 username_
                          , "\n"]
      sendAll soc $ msg
    Just user ->
      sendAll soc (formatUser user)

formatUser :: User -> ByteString
formatUser (User _ username_ shell_
            homeDir realName_ _) =
  BS.concat ["Login: ", e username_, "\t\t\t\t",
             "Name: ", e realName_, "\n",
             "Directory: ", e homeDir, "\t\t\t",
             "Shell: ", e shell_, "\n"]
  where e = encodeUtf8

handleQuery :: SQLite.Connection
            -> Socket
            ->IO()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name -> returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Socket
              -> SQLite.Connection
              -> IO()
handleQueries sock dbConn = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  Network.Socket.close soc

daemon :: Int -> (Socket -> IO ()) ->  IO ()
daemon port action = withSocketsDo $ do
  bracket open close action
  where open = do
          addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just $ show port)
          let serveraddr = head addrinfos
          sock <- socket (addrFamily serveraddr)
                  Stream defaultProtocol
          Network.Socket.bind sock (addrAddress serveraddr)
          listen sock 1
          return sock

withDb :: (SQLite.Connection -> IO ()) -> IO ()
withDb = bracket
  (SQLite.open "finger.db")
  SQLite.close

addUser' :: Socket -> SQLite.Connection -> IO ()
addUser' = undefined

fingerd :: IO ()
fingerd = withSocketsDo $
  daemon 79 $ withDb . handleQueries

addd :: IO ()
addd = withSocketsDo $
  daemon 79 $ withDb . addUser'


main :: IO ()
main = do
  _ <- forkIO fingerd
  addd
  return ()
