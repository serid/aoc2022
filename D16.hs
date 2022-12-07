{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module D16 where

import Data.Maybe
import Data.List
import Data.Functor
import qualified Data.Map as Map

import Flow
import Data.Either.Combinators
import Data.Array.IArray
import Data.Array.Unboxed
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Control.Monad.Trans.State.Strict
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH

import Lib

type Graph = Array GraphId Node
type GraphId = Int
data Node = Node Int (UArray Int GraphId) deriving Show

type IdMap = Map.Map String GraphId
type D16Parsec a = MyParsec ParserState a
data ParserState = ParserState {
    _nextGraphId :: !GraphId,
    _graphIdMap :: !IdMap
}
makeLenses ''ParserState

newId :: State ParserState GraphId
newId = do
    id <- get <&> (^. nextGraphId)
    modify (nextGraphId %~ (+1))
    return id

readNeighs :: [String] -> State ParserState [GraphId]
readNeighs (x:xs) = get >>= \m -> case Map.lookup x (m ^. graphIdMap) of
    -- Add the name
    Nothing -> do
        id <- newId
        return (id:readNeighs xs)

lineParser :: D16Parsec Node
lineParser = do
    id <- applyState newId

    idMap <- getState <&> (^. graphIdMap)

    string "Valve "
    name <- identP
    string " has flow rate="
    rate <- numP
    --return $! myTrace name
    string "; tunnels lead to valves " <|%> string "; tunnel leads to valve "
    neighs <- map (\k -> fromJust $ myTrace $ Map.lookup k idMap) <$> listP
    string "\n"

    modifyState (graphIdMap %~ insertAssert name id)

    return $ Node id $ arrayFromList neighs

parser :: D16Parsec Graph
parser = arrayFromList <$> many lineParser

parseGraph :: String -> Graph
parseGraph s =
    let res = runParser parser (ParserState 0 Map.empty) "" s in
    unwrapEither $ mapLeft show $ res

star1 :: String -> String
star1 s =
    let g = parseGraph s in
    show g
    --g `seq` ""

star2 :: String -> String
star2 s = undefined

algos = (star1, star2)
