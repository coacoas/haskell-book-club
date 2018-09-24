module Exercises where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

rDec :: Num a => Reader a a
rDec = ReaderT $ \i -> return (i - 1)

rDec' :: Num a => Reader a a
-- rDec' = ReaderT $ fmap (subtract 1) . pure
rDec' = ReaderT $ return . (subtract 1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \i -> do
  putStrLn("Hi: " <> show i)
  return $ i + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStrLn("Hi: " <> show s)
  return $ (show s, s + 1)


-- 
-- Make the code work
--
  
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO $ getLine
  guard $ isValid v
  return v


doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn("Good, was very excite: " ++ e)
