{-
The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
-}

import Data.List

-- Kind of inefficient.
-- Idea: Generate numbers instead of checking primes.
--       Numbers can start with 2, 3, 5, 7
--       Middle can be 1, 3, 7, 9
--       End can be 3, 7, 9
--       Interesting functions: tails, concatMap, all
--       Maybe use div and mod instead of strings? 
--       Probably better with isPrime as trialDivision than sieve. 
main = print . sum . take 11 $ getNumbers

getNumbers :: [Int]
getNumbers = [nb | nb <- primes,
                   nb > 10,
                   let s = show nb,
                   not (elem '4' s),
                   not (elem '6' s),
                   not (elem '8' s),
                   leftTruncatable s,
                   rightTruncatable s]

leftTruncatable :: String -> Bool
leftTruncatable [] = True
leftTruncatable (x:xs) = leftTruncatable xs && elem (read (x:xs)) primes 
                   
rightTruncatable :: String -> Bool
rightTruncatable [] = True
rightTruncatable nb = rightTruncatable (init nb) && elem (read nb) primes 

primes :: [Int]
primes = sieve 1000000 [2..1000000]

sieve :: Int -> [Int] -> [Int]
sieve _ [] = []
sieve limit (x:xs)
    | fromIntegral x < sqrt (fromIntegral limit) = x : sieve limit (removeMultiples (x:xs))
    | otherwise = (x:xs)

removeMultiples :: [Int] -> [Int]
removeMultiples list = filter (\ a -> mod a (head list) /= 0) list
