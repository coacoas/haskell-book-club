{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module StoppingTheParty where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import System.Random (randomRIO)

data StopException where StopException :: Exception e => e -> StopException

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Int)
  if i `elem` [1..9]
    then throwIO $ StopException DivideByZero
    else throwIO $ StopException StackOverflow
    
main :: IO ()
main = forever $ do
  let tryS :: IO () -> IO (Either StopException ())
      tryS = try
  _ <- tryS randomException
  putStrLn "Live to loop another day!" -- microseconds
  threadDelay (1 * 1000000)
