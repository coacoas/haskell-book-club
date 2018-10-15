module V where

import Data.Validation
import Control.Applicative
import Data.Traversable
import Data.List

bad :: [Validation [String] Integer]
bad = [(Success 3), (Failure ["uh oh"]), (Failure ["No good"]), (Success 5)]

good :: [Validation [String] Integer]
good = [(Success 3), (Success 4), (Success 123), (Success 5)]

run :: [Validation [String] Integer] -> IO ()
run xs = 
  let out :: Validation [String] [Integer]
      out = traverse id xs
  in print $ out
