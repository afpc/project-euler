{-
It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
-}

import Data.List

main = print . head $ permutedMultiples

permutedMultiples :: [Int]
permutedMultiples = [nb | nb <- [1..],
                          sameDigits (show nb) (show $ nb*2),
                          sameDigits (show nb) (show $ nb*3),
                          sameDigits (show nb) (show $ nb*4),
                          sameDigits (show nb) (show $ nb*5),
                          sameDigits (show nb) (show $ nb*6)]

sameDigits :: String -> String -> Bool 
sameDigits a b = sort a == sort b
