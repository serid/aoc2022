module Aocs where

import qualified D1
import qualified D16

type List2 a = (a, a)
type Aoc = List2 (String -> String)

aocs :: Int -> Maybe Aoc
aocs 1 = Just D1.algos
aocs 16 = Just D16.algos
aocs _ = Nothing
