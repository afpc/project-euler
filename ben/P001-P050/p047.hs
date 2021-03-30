{-
The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?
-}

import Data.List

-- Fairly inefficient.
main = print . head $ findNbs

findNbs :: [Int]
findNbs = [nb | nb <- [1..],
                nbDistinctPrimes nb == 4, 
                nbDistinctPrimes (nb + 1) == 4, 
                nbDistinctPrimes (nb + 2) == 4, 
                nbDistinctPrimes (nb + 3) == 4] 

nbDistinctPrimes :: Int -> Int
nbDistinctPrimes nb = length . nub . primeFactorisation $ nb

findSmallestDivisor :: Int -> Int -> Int
findSmallestDivisor start nb 
    | mod nb start == 0 = start
    | start > round (sqrt (fromIntegral nb)) = nb
    | otherwise = findSmallestDivisor (start + 1) nb

primeFactorisation :: Int -> [Int]
primeFactorisation nb 
    | smallest == nb = [smallest]
    | otherwise = smallest : (primeFactorisation (div nb smallest)) 
    where smallest = findSmallestDivisor 2 nb
