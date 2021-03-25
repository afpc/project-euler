{-
The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

d2d3d4=406 is divisible by 2
d3d4d5=063 is divisible by 3
d4d5d6=635 is divisible by 5
d5d6d7=357 is divisible by 7
d6d7d8=572 is divisible by 11
d7d8d9=728 is divisible by 13
d8d9d10=289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property.
-}

import Data.Char
import Data.List

main = print . sum $ generateNb

generateNb :: [Int]
generateNb = [x | x <- (panDigitalNb 9), 
                  mod (subString 2 3 x) 2 == 0,  
                  mod (subString 3 3 x) 3 == 0,  
                  mod (subString 4 3 x) 5 == 0,  
                  mod (subString 5 3 x) 7 == 0,  
                  mod (subString 6 3 x) 11 == 0,  
                  mod (subString 7 3 x) 13 == 0,  
                  mod (subString 8 3 x) 17 == 0]    

panDigitalNb :: Int -> [Int]
panDigitalNb nb = map (foldl1 (\x y -> 10*x+y)) (permutations [0..nb])

subString :: Int -> Int -> Int -> Int
subString begin len nb = read substr
    where substr = take len (drop (begin - 1) str)
          str = show nb
