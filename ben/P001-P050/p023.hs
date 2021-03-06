{-
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
-}

import Data.List
import qualified Data.Set as Set

main = print (sum [1..limit] - sum abundantSumNbs)

-- Improved limit.
limit :: Int
limit = 20161

-- Conditions for specific numbers

perfect :: Int -> Bool
perfect nb = sum (divisors nb) == nb

abundant :: Int -> Bool
abundant nb = sum (divisors nb) > nb

deficient :: Int -> Bool
deficient nb = sum (divisors nb) < nb

-- Lists of different categories of numbers 

perfectNbs :: [Int]
perfectNbs = [n | n <- [1..limit],
                  perfect n]

abundantNbs :: [Int]
abundantNbs = [n | n <- [1..limit],
                   abundant n]

deficientNbs :: [Int]
deficientNbs = [n | n <- [1..limit],
                    deficient n]

-- Calculating specific numbers that are a sum of two abundant numbers or not. 

abundantSumNbs :: [Int]
abundantSumNbs = unique [n | n1 <- abundantNbs,
                             n2 <- abundantNbs,
                             n2 >= n1, 
                             let n = n1 + n2, 
                             n <= limit]

-- Help functions.
 
unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

divisors :: Int -> [Int]
divisors 1 = []
divisors nb = let limit = floor (sqrt (fromIntegral nb))
              in (1:) . nub . concat $ [[x, div nb x] | x <- [2..limit],
                                                        mod nb x == 0]
