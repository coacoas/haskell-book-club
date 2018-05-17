module Main where

import System.IO
import Caesar
import Prompt

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  rotate <- prompt "How much will you rotate?"
  message <- prompt "What is your message?"
  let rot = (read :: String -> Int) rotate
  putStrLn $ caesar rot message
