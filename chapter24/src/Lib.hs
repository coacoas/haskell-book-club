module Lib where

import Text.Trifecta
import Control.Applicative
import Control.Monad

stop :: Show a => Parser a
stop = unexpected "stop"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

one'' :: Parser Char
one'' = one <* eof

-- read two characters, '1', and '2'
oneTwo :: Parser Char
oneTwo = char '1' >> char '2'
-- read two characters, -- '1' and '2', then die

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

oneTwo'' :: Parser Char
oneTwo'' = oneTwo <* eof

oneTwoThree :: Parser Char
oneTwoThree = char '1' >> char '2' >> char '3'

oneTwoThree' :: Parser Char
oneTwoThree' = oneTwoThree <* eof

oneTwoThreeString :: Parser String
oneTwoThreeString = string "123" <|> string "12" <|> string "1"

-- Doesn't show the last character parsed because of `mempty`, but not sure how to fix that
string' :: String -> Parser String
string' = foldr parseOne mempty
  where
    parseOne :: Char -> Parser String -> Parser String
    parseOne c p = char c >> p

string'' :: String -> Parser String
string'' = traverse char

-- Third time's the charm!
string''' :: String -> Parser String
string''' (x:xs) = do
  c <- char x
  s <- string''' xs
  return  $ c : s
string''' [] = pure []
  
strToInt :: Parser Integer
strToInt = do
  i <- integer
  eof
  return i

strToInt' :: Parser Integer
strToInt' = integer <* eof
