module Palindrome where

import Control.Monad
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let lower = map toLower line1
      normalized = filter isLower lower
  case (normalized == reverse normalized) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"
