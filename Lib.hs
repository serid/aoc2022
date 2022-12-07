module Lib where

import Data.Maybe
import Debug.Trace
import Control.Exception hiding (try)
import qualified Data.Map as Map

import Data.Array.IArray
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Control.Monad.Trans.State.Strict
import Data.Profunctor

myTrace :: Show a => a -> a
myTrace x = trace ("doge: " ++ (show x)) x

--tryInit :: [a] -> [a]
--tryInit [] = []
--tryInit xs = init xs

unwrapEither :: Either String a -> a
unwrapEither (Left e) = error e
unwrapEither (Right x) = x

arrayFromList :: (IArray a e) => [e] -> a Int e
arrayFromList xs = listArray (0, length xs - 1) xs

-- Insert and assert that key was not present
insertAssert :: Ord k => k -> a -> Map.Map k a -> Map.Map k a
insertAssert k x m = (isJust $ Map.lookup k m) `assert` Map.insert k x m


-- Simple parsers:

identP :: MyParsec s String
identP = many1 letter

numP :: MyParsec s Int
numP = fmap read $ many1 digit

-- Parses "A, B, C"
listP :: MyParsec s [String]
listP = do
    first <- identP
    tail <- (string ", " >> listP) <|> return []
    return (first:tail)

-- Backtracking choice
infixl 3 <|%>
(<|%>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
(<|%>) = lmap try (<|>)
-- choice2 p q = try p <|> q

applyState :: State u a -> Parsec s u a
applyState o = do
    st <- getState
    let (x, s) = runState o st
    setState s
    return x

type MyParsec s a = Parsec String s a
