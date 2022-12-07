{-# LANGUAGE ScopedTypeVariables #-}

module D1 where

import Data.List
import Data.Maybe

import Flow

import Lib

algos = (star1, star2)

star1 :: String -> String
star1 s = aoc1 (show . maximum) s

star2 :: String -> String
star2 s =
    let folder = \sorted num -> updateLTInSorted num sorted in
    let consumer = foldl folder [-1, -1, -1] in
    aoc1 (show . (\res -> (res, sum res)) . consumer) s

aoc1 :: ([Int] -> String) -> String -> String
aoc1 consumer s =
    let groupF = 
            let ints = map read in
            let sums = sum . ints in
            sums in

    let ls = lines s in
    let groups = groupBy (\x y -> x /= "" && y /= "") ls in
    let noEmpty = filter (/= [""]) groups in
    let groups2 = map groupF noEmpty in
    consumer groups2

-- Find the first element that is less than and replace it
-- List is sorted in non-strict decreasing order
updateLTInSorted :: Int -> [Int] -> [Int]
updateLTInSorted n [] = []
updateLTInSorted n (x:xs) = if x < n then n:tryShift x xs else x:updateLTInSorted n xs

{-
    let folder = \set (num :: Int) -> do {
        smaller <- Set.lookupLT num set;
        let { newSet = Set.delete smaller set };
        return $ Set.insert num newSet } |> fromMaybe set
        in
    let consumer = foldl folder (Set.fromList [-1, -2, -3]) in
    aoc1 (show . sum . Set.toList . consumer) s
    -}

-- Try to shift a new element to head while keeping list length unchanged
tryShift :: a -> [a] -> [a]
tryShift x [] = []
tryShift x xs = x:init xs
