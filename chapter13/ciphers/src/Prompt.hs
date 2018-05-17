module Prompt where

prompt :: String -> IO String
prompt p = do
  putStr $ p ++ " "
  getLine
