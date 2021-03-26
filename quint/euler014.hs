-- The following iterative sequence is defined for the set of positive integers:
-- n → n/2    (n is even)
-- n → 3n + 1 (n is odd)
-- Which starting number, under one million, produces the longest chain?

import Data.List

next :: Int -> [Int]
next n
  | n == 1    = [1]
  | even n    = n : (next (div n 2))
  | otherwise = n : (next (3 * n + 1))
  
collatz :: [Int] -> [(Int,Int)]
collatz []     = []
collatz (x:xs) = ((length (next x)),x) : (collatz xs)

main :: IO ()
main = putStrLn (show (snd (last (sort (collatz [1..1000000])))))

