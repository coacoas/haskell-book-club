{-# LANGUAGE DeriveAnyClass #-}

module OurExceptions where

import Control.Exception

data NotDivThree = NotDivThree deriving (Eq, Show, Exception)

-- instance Exception NotDivThree

data NotEven = NotEven deriving (Eq, Show, Exception)

-- instance Exception NotEven
