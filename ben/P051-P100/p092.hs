{-
Square digit chains.

A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.

For example,

44 → 32 → 13 → 10 → 1 → 1
85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

How many starting numbers below ten million will arrive at 89?
-}

import qualified Data.Map as Map
import Data.Ord
import Data.List
import Data.Function

-- Pure brute force
--main = print . length . filter (==True) $ iteration

next :: Int -> Int 
next n = sum list
    where list = map (\x -> (read [x] :: Int)^2) (show n)

produceChain :: Int -> Bool
produceChain 1 = False
produceChain 89 = True
produceChain nb = produceChain (next nb)

iteration :: [Bool]
iteration = [(produceChain n) | n <- [1..10000000]]

-- Somewhat smarter

main = print . length . filter (== 89) . calculate [1..10000000] $ startMap

maxNb :: Int 
maxNb = 7*9^2

startMap :: Map.Map Int Int
startMap = Map.fromList [(1, 1), (89,89)]

calculate :: [Int] -> Map.Map Int Int -> [Int]
calculate [] _ = []
calculate (x:xs) m = end : calculate xs newMap
    where end = getEnd x m
          newMap = if end <= maxNb
                       then Map.insert x end m
                   else m

getEnd :: Int -> Map.Map Int Int -> Int
getEnd nb m
    | Map.member nb m = m Map.! nb
    | otherwise = getEnd (next nb) m

-- Third try 

--main = print . sum . map snd . filter (\ (a,_) -> a == 89) . calculate' (firstLoop 0) $ startMap

firstLoop :: Int -> [(Int,Int)]
firstLoop 10000 = []
firstLoop nb = folded
    where folded = map (foldl1 (\ (a,b) (_,d) -> (a,b+d))) grouped
          grouped = groupBy ((==) `on` fst) sorted
          sorted = sortBy (comparing fst) (newList ++ oldList)
          newList = [(next (nb * 1000 + n),1) | n <- [1..1000]]
          oldList = firstLoop (nb + 1)

calculate' :: [(Int,Int)] -> Map.Map Int Int -> [(Int,Int)]
calculate' [] _ = []
calculate' ((a,b):xs) m = (end, b) : calculate' xs newMap
    where end = getEnd a m
          newMap = Map.insert a end m
