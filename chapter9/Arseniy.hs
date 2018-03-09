module Arseniy where

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ []  = []
splitBy splitter str =
  case str of
    (s : tail) | s == splitter  ->  splitBy splitter tail
               | otherwise      ->  (takeWhile (/= splitter) str) :
                                       (splitBy splitter (dropWhile (/= splitter) str))

myWords2 :: String -> [String]
myWords2   = splitBy ' '

myWords :: String -> [String]
myWords [] = []
myWords (' ' : tail) = myWords tail
myWords str = (takeWhile (/=' ') str) : (myWords (dropWhile (/= ' ') str))
