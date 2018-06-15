module Lib where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ fmap f ma

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
    putStrLn ("y helo thar: " ++ name)

twiceWhenEven::[Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

filter' :: (Integer -> Bool) -> [Integer] -> [Integer]
filter' p xs = do
  x <- xs
  if (p x)
    then [x]
    else []
