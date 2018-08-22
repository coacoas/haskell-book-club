import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

import Control.Applicative
import Data.Monoid

import Exercises

type Instances = (Int, Int, [Int])

main = do
  quickBatch $ traversable $ (undefined :: Identity Instances)
  quickBatch $ traversable $ (undefined :: Constant Instances Instances)
  quickBatch $ traversable $ (undefined :: Optional Instances)
  quickBatch $ traversable $ (undefined :: List Instances)
  quickBatch $ traversable $ (undefined :: Three Int String Instances)
  quickBatch $ traversable $ (undefined :: Pair String Instances)
  quickBatch $ traversable $ (undefined :: Big Int Instances)
  quickBatch $ traversable $ (undefined :: Bigger Int Instances)
--   quickBatch $ traversable $ (undefined :: S Maybe Instances)
