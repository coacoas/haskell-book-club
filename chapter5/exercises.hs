module Arith3Broken where

munge :: (x -> y) 
      -> (y -> (w, z))
      -> x
      -> w
munge xy ywz x = w
      where (w, _)= ywz(xy(x))

main :: IO()
main = do
  print (1 + 2)
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
    where blah = negate 1

f :: (a -> b) -> a -> b
f = id
