{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.IO (Handle, hPutStr, hGetChar, hGetContents, hIsEOF, hWaitForInput, stdout, stdin)
import qualified Cipher
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Operation = Encrypt | Decrypt deriving (Show, Ord, Eq)
data Config = Config { operation :: Operation
                     , key :: String
                     } deriving (Show, Eq)

loadConfig :: [String] -> IO Config
loadConfig (["-d", key]) = pure $ Config Decrypt key
loadConfig (["-e", key]) = pure $ Config Encrypt key
loadConfig _             = printUsage >> fail "Invalid usage"

printUsage :: IO ()
printUsage = putStrLn("Argumengs: (-d|-e) key")

readContents :: Handle -> IO String
readContents h = go "" h
  where go acc h' = do
          done <- hIsEOF h'
          if (done) then return $ reverse acc
            else do c <- hGetChar h'
                    go (c : acc) h'
            

main :: IO ()
main = do
  args           <- getArgs
  Config op key  <- loadConfig args
  message        <- hGetContents stdin -- readContents stdin
  hPutStr stdout $ action op key message
  where action Encrypt = Cipher.encrypt
        action Decrypt = Cipher.decrypt

