module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import RandomExample

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- replicateM :: Monad m => Int->ma->m[a]

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 [] 0 g
  where
    go :: Int -> [Die] -> Int -> StdGen -> (Int, [Die])
    go sum dice count gen
      | sum >= n = (count, dice)
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
          in go (sum + die) (intToDie die : dice) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n =  fst . rollsCountLogged n

rollsToGetTwenty :: StdGen -> (Int, [Die])
rollsToGetTwenty = rollsCountLogged 20
