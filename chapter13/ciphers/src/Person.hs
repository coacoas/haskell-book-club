module Person where

import Prompt

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                "Name was: " ++ show name ++
                " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  name <- prompt "Name?"
  ageS <- prompt "Age?"
  let age = (read :: String -> Integer) ageS
      result = case (mkPerson name age) of
                 Right p -> "Yay! Successfully got a Person! " ++ show p
                 Left e  -> "An error occurred! " ++ show e
  putStrLn result
