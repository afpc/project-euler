-- Find the sum of all the multiples of 3 or 5 below 1000.

import Data.List

solution :: Int -> Int
solution nb = sum . nub $ [x | x <- [1..(nb-1)], y <- [3,5] , mod x y  == 0]
