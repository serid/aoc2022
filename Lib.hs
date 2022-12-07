module Lib where

import Debug.Trace

myTrace :: Show a => a -> a
myTrace x = trace ("doge: " ++ (show x)) x

tryInit :: [a] -> [a]
tryInit [] = []
tryInit xs = init xs
