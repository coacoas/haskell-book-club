module IntParser where

import Control.Applicative ((<|>))
import Data.Char (digitToInt, ord)
import Data.List (foldl')
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = do
  let rollup :: Integer -> Char -> Integer
      rollup base c = base * 10 + (toInteger $ digitToInt c)
  digits <- some parseDigit
  return $ foldl' rollup 0 digits

base10Integer' :: Parser Integer
base10Integer' = 
  let negative = negate <$> (char '-' *> base10Integer)
      positive = (optional $ char '+') *> base10Integer
  in negative <|> positive
        
