{-
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?
-}

import qualified Data.Map as Map

limit :: Int 
limit = 20

start :: (Int,Int)
start = (0,0)

end :: (Int,Int)
end = (limit,limit)

-- Brute Force -> too long
-- main = print . length . paths $ [start]

paths :: [(Int,Int)] -> [Bool]
paths [] = [] 
paths ((a,b):xs) 
    | a > limit = paths xs
    | b > limit = paths xs
    | (a,b) == end = True :(paths xs)
    | otherwise = paths ((a+1,b):(a,b+1):xs)

-- Calculate each node only once

main = print calculate

calculate :: Int
calculate = (posPath allPositions mapping) Map.! (0,0) 

posPath :: [(Int,Int)] -> Map.Map (Int,Int) Int -> Map.Map (Int,Int) Int
posPath [] m = m
posPath ((a,b):xs) m = posPath xs newMap
    where newMap = Map.insert (a,b) value m
          value = f (a+1,b) m + f (a,b+1) m
          
          f :: (Int,Int) -> Map.Map (Int,Int) Int -> Int
          f (a,b) m
              | a > limit = 0
              | b > limit = 0
              | (a,b) == end = 1
              | otherwise = m Map.! (a,b)

allPositions :: [(Int,Int)]
allPositions = [(i,j) | i <- [limit,limit-1..0],
                        j <- [limit,limit-1..0]]

mapping :: Map.Map (Int,Int) Int
mapping = Map.fromList . map (\(a,b) -> ((a,b),0)) $ allPositions
