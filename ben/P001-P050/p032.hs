{-
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
-}

import Data.List

main = print . sum . nub $ generateNbs

generateNbs :: [Int]
generateNbs = [prod | n1 <- [2..98],
                      n2 <- [123..9876],
                      let prod = n1 * n2,
                      isPanDigital (show n1 ++ show n2 ++ show prod)]

isPanDigital :: String -> Bool
isPanDigital nb = sort nb == "123456789"
