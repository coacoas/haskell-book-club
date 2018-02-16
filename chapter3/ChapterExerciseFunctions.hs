module ChapterExerciseFunctions where

excurry :: String
excurry = "Curry is awesome" ++ "!"

exfirstWordLastLetter :: String
exfirstWordLastLetter = take 1 $ drop 4 "Curry is awesome"

exLastWord :: String
exLastWord = drop 9 "Curry is awesome!"

ex :: String -> String
ex s = s ++ "!"

lastLetterFirstWord :: String -> String
lastLetterFirstWord s = let
     firstWord = takeWhile (\c -> c /= ' ') s
  in drop (length firstWord - 1) firstWord

lastWord :: String -> String
lastWord = drop 9

letterIndex :: Int -> String -> Char
letterIndex i s = s !! (i - 1)

letterIndex' :: Int -> Char
letterIndex' i = "Curry is awesome!" !! (i - 1)

thirdLetter :: String -> Char
thirdLetter s = letterIndex 3 s

rvrs :: String -> String
rvrs s =
  let 
     frst = take 5 s
     mddl = take 2 $ drop 6 s
     lst = take 7 $ drop 9 s
  in lst ++ " " ++ mddl ++ " " ++ frst

rvrs' :: String -> String
rvrs' str = concat [
  drop 9 str,
  take 4 $ drop 5 str,
  take 5 str]

main :: IO ()
main = do
  print excurry
  print exfirstWordLastLetter
  print exLastWord
  putStrLn "-----"
  print $ ex "Curry is awesome"
  print $ lastLetterFirstWord "Curry is awesome!"
  print $ lastWord "Curry is awesome!"
  putStrLn "-----"
  print $ thirdLetter "Curry is awesome!"
  print $ thirdLetter "abcdefg!"
  print $ rvrs "Curry is awesome"
  
  

