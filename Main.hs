module Main where

import System.Environment
import Text.Read

import Safe
import Data.Either.Extra
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Aocs

main1 :: ExceptT String IO String
main1 = do
  args <- liftIO $ getArgs
  selector <- hoistEither $ (args `atMay` 0) `expect`
    "Expected one command-line argument: AOC day selector"
  selectorN <- hoistEither $ readEither selector
  aoc <- hoistEither $ (aocs selectorN) `expect`
    "AOC day selector out of bounds"
  
  input <- liftIO $ readFile $ "input/" ++ selector ++ ".txt"

  let output = fmt "Star 1: %\nStar 2: %" [fst aoc $ input, snd aoc $ input]
  return output

main :: IO ()
main = do
  output <- runExceptT main1
  putStrLn $ fromEither output
  return ()


expect :: Maybe a -> e -> Either e a
expect Nothing = Left
expect (Just x) = const $ Right x

hoistEither :: Monad m => Either e a -> ExceptT e m a
hoistEither (Left e) = ExceptT $ return (Left e)
hoistEither (Right x) = return x

fmt :: String -> [String] -> String
fmt [] _ = []
fmt ('%':xs) [] = "error: argument expected"
fmt ('%':xs) (arg:args) = arg ++ fmt xs args
fmt (x:xs) args = x:fmt xs args
