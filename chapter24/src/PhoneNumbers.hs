module PhoneNumbers where

import Control.Applicative ((<|>))
import Data.Char (digitToInt, ord)
import Text.Trifecta

-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

digitAsInt :: Parser Int
digitAsInt = digitToInt <$> digit

threeDigits :: Parser Int
threeDigits = do
  a <- digitAsInt
  b <- digitAsInt
  c <- digitAsInt
  return $ (a * 100) + (b * 10) + c

fourDigits :: Parser Int
fourDigits = do
  a <- digitAsInt
  three <- threeDigits
  return $ (a * 1000) + three

parseNPA = threeDigits
parseExchange = threeDigits
parseLineNumber = fourDigits

parseWithDashes = do
  npa <- parseNPA
  _ <- char '-'
  ex  <- parseExchange
  _ <- char '-'
  num <- parseLineNumber
  return $ PhoneNumber npa ex num
  
parseWithoutDashes = do
  npa <- parseNPA
  ex  <- parseExchange
  num <- parseLineNumber
  return $ PhoneNumber npa ex num

parseWithParentheses = do
  _   <- char '('
  npa <- parseNPA
  _   <- char ')'
  _   <- some $ char ' '
  ex  <- parseExchange
  _ <- char '-'
  num <- parseLineNumber
  return $ PhoneNumber npa ex num

parseWithLeading =
  string "1-" *> parseWithDashes

parsePhone :: Parser PhoneNumber
parsePhone =
  (try parseWithDashes)
  <|> (try parseWithoutDashes)
  <|> (try parseWithParentheses)
  <|> parseWithLeading
