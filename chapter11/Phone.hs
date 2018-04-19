module Phone where

import Data.Char
import Data.List
import Data.Maybe

data DaPhone = DaPhone { keys :: [String] } deriving Show

daPhone :: DaPhone
daPhone = DaPhone
          ["+_0"
          ,"1"
          ,"ABC2"
          ,"DEF3"
          ,"GHI4"
          ,"JKL5"
          ,"MNO6"
          ,"PQRS7"
          ,"TUV8"
          ,"WXYZ9"
          ,"^*"
          ,".,#"
          ]
          
convo :: [String]
convo =
  ["Wanna play 20 questions"
  ,"Ya"
  ,"U 1st haha"
  ,"Lol ok. Have u ever tasted alcohol"
  ,"Lol ya"
  ,"Wow ur cool haha. Ur turn"
  ,"Ok. Do u think I am pretty Lol"
  ,"Lol ya"
  ,"Just making sure rofl ur turn"
  ]
  
-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps phone c
  | isUpper c = [('*', 1)] ++ reverseTaps phone (toLower c)
  | otherwise = let
      key = findIndex (elem c) (keys phone)
      keyDigit i
        | i < 10 = chr $ ord '0' + i
        | i == 10 = '*'
        | i == 11 = '#'
      presses = elemIndex c (keys phone !! (fromJust key))
      in [(keyDigit (fromJust key), fromJust presses)]
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead phone msg = concat . map (reverseTaps phone) $ msg
