-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

import Data.Set as S

unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

minus :: (Ord a) => [a] -> [a] -> [a]
minus xs [] = xs
minus [] _ = []
minus x'@(x:xs) y'@(y:ys) = case compare x y of
  LT -> x : minus xs y'
  EQ -> minus xs ys
  GT -> minus x' ys

divisors :: Int -> [Int]
divisors n = [x | x <- [1..(n-1)], rem n x == 0]

abundant :: [Int]
abundant = [x | x <- [1..28123], x < sum (divisors x)]

sums :: [Int]
sums = unique [x + y | x <- abundant, y <- abundant, x + y <= 28123]

main :: IO ()
main = print (sum (minus [1..28123] sums))

