{-
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence? 
-}

-- Needs cleaning up!
-- Not sure if I did something wrong? since I got more terms when not using 3330 as difference.
-- Maybe better to generate all permutations of one prime, calculate the difference and check when you add that difference once more, if that is a prime as well?

import Data.List

main = print . head . filter (/= "148748178147") . generateAnswer $ generateTriples

generateAnswer :: [[Int]] -> [String]
generateAnswer = nub . foldr (\a b -> [concat (map (show) a)] ++ b) []

generateTriples :: [[Int]]
generateTriples = [answer | x <- usefulPrimes,
                            let list = nub . permutations . show $ x,
                            let nbs = map (\a -> read a :: Int) list,
                            let filtered = filter (\a -> elem a usefulPrimes) nbs,
                            length filtered >= 3,
                            let diff = findEqualDiff filtered,
                            length diff == 2,
                            let answer = nub . sort $ [fst (diff!!0), snd (diff!!0), fst (diff!!1), snd (diff!!1)]]

findEqualDiff :: [Int] -> [(Int,Int)]
--findEqualDiff input = filter (\a -> length a == 2) . group . sort $ dif
findEqualDiff input = dif
    where len = length input
          dif = [(input!!x, input!!y) | x <- [0..(len-2)],
                        y <- [(x+1)..(len-1)],
                        3330 == abs (input!!x - input!!y)]

usefulPrimes :: [Int]
usefulPrimes = dropWhile (<1000) primes

primes :: [Int]
primes = sieve 9999 [2..9999]

sieve :: Int -> [Int] -> [Int]
sieve _ [] = []
sieve limit (x:xs)
    | fromIntegral x < sqrt (fromIntegral limit) = x : sieve limit (removeMultiples (x:xs))
    | otherwise = (x:xs)

removeMultiples :: [Int] -> [Int]
removeMultiples list = filter (\ a -> mod a (head list) /= 0) list
