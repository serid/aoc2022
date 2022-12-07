module Aocs where

import D1

type List2 a = (a, a)
type Aoc = List2 (String -> String)

aocs :: [Aoc]
aocs = [(aoc1x1, aoc1x2)]
