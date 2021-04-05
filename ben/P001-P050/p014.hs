{-
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
-}

import qualified Data.Map as Map
import Data.Ord
import Data.List

-- Brute force
-- main = print . maximum . map (length) $ iteration

next :: Int -> Int 
next n 
    | even n = div n 2
    | otherwise = 3*n + 1

produceChain :: Int -> [Int]
produceChain 1 = []
produceChain nb = (next nb) : produceChain (next nb)

iteration :: [[Int]]
iteration = [(produceChain n) | n <- [1..1000000]]

-- Somewhat smarter

main = print . fst . maximumBy (comparing snd) . Map.toList . calculate [1..1000000] $ Map.empty

calculate :: [Int] -> Map.Map Int Int -> Map.Map Int Int
calculate [] m = m
calculate (x:xs) m = calculate xs (Map.insert x (getLength x m) m)

getLength :: Int -> Map.Map Int Int -> Int
getLength 1 m = 1
getLength nb m
    | Map.member nb m = 1 + m Map.! nb
    | otherwise = 1 + getLength (next nb) m
