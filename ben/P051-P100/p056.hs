{-
Powerful digit sum.

A googol (10^100) is a massive number: one followed by one-hundred zeros; 100100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?
-}

import Data.Char

main = print . maximum $ digitalSums

digitalSums :: [Int]
digitalSums = [s | a <- [1..100],
                   b <- [1..100],
                   let result = a^b,
                   let s = sum . map digitToInt . show $ result] 
