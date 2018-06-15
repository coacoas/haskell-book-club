module Lookup1 where

import Control.Applicative

added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
added' = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
