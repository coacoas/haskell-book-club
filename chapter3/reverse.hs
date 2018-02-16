module Reverse where

rvrs :: String -> String
rvrs s = lst ++ " " ++ mddl ++ " " ++ frst
  where frst = take 5 s
        mddl = take 2 $ drop 6 s
        lst = take 7 $ drop 9 s

main :: IO ()
main = print (rvrs "Curry is awesome")
