{-
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: As 1! = 1 and 2! = 2 are not sums they are not included.
-}

{- Thought process: 

The biggest digit is 9 which results in 9! = 362880.
So numbers bigger than 7 (maybe even 6) digits will never be able to fullfil the requirements, which we can use as a search limit. 
-}

import Data.Char

limit :: Int 
limit = 7 * (fac 9)

main = print . sum $ curiousNumbers

curiousNumbers :: [Int]
curiousNumbers = [x | x <- [3..limit], 
                            (sum . map (fac . digitToInt) . show $ x) == x]

fac :: Int -> Int
fac nb = product [1..nb]
