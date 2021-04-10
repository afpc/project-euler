{-
A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
-}

import Data.List

-- Bruteforce 
--main = print . head . drop 999998 $ brute
--main = print . head . drop (999998 - (2 * fac 9 + 6 * fac 7)) $ kindaBrute

brute :: [[Int]]
brute = [a:b:c:d:e:f:g:h:i:[j] | a <- [0..9],
                                 b <- [0..9],
                                 c <- [0..9],
                                 d <- [0..9],
                                 e <- [0..9],
                                 f <- [0..9],
                                 g <- [0..9],
                                 h <- [0..9],
                                 i <- [0..9],
                                 j <- [0..9],
                                 not . elem a $ (b:c:d:e:f:g:h:i:[j]),
                                 not . elem b $ (a:c:d:e:f:g:h:i:[j]),
                                 not . elem c $ (a:b:d:e:f:g:h:i:[j]),
                                 not . elem d $ (a:b:c:e:f:g:h:i:[j]),
                                 not . elem e $ (a:b:c:d:f:g:h:i:[j]),
                                 not . elem f $ (a:b:c:d:e:g:h:i:[j]),
                                 not . elem g $ (a:b:c:d:e:f:h:i:[j]),
                                 not . elem h $ (a:b:c:d:e:f:g:i:[j]),
                                 not . elem i $ (a:b:c:d:e:f:g:h:[j]),
                                 not . elem j $ (a:b:c:d:e:f:g:h:[i])]

-- We can slink the possibilities down a little bit if we use a little bit of combinatorics. We can calculate all possible permutations of 10 numbers with the formula 10!. So we can calculate that a will be 2, since 2*9! < 1000000 and 3*9! > 1000000 and so on for the following numbers.
kindaBrute :: [[Int]]
kindaBrute = [1:5:c:d:e:f:g:h:i:[j] | c <- [0..9],
                                      d <- [0..9],
                                      e <- [0..9],
                                      f <- [0..9],
                                      g <- [0..9],
                                      h <- [0..9],
                                      i <- [0..9],
                                      j <- [0..9],
                                      not . elem c $ (1:5:d:e:f:g:h:i:[j]),
                                      not . elem d $ (1:5:c:e:f:g:h:i:[j]),
                                      not . elem e $ (1:5:c:d:f:g:h:i:[j]),
                                      not . elem e $ (1:5:c:d:f:g:h:i:[j]),
                                      not . elem f $ (1:5:c:d:e:g:h:i:[j]),
                                      not . elem g $ (1:5:c:d:e:f:h:i:[j]),
                                      not . elem h $ (1:5:c:d:e:f:g:i:[j]),
                                      not . elem i $ (1:5:c:d:e:f:g:h:[j]),
                                      not . elem j $ (1:5:c:d:e:f:g:h:[i])]

smarter :: [[Int]]
smarter = [a:b:c:d:e:f:g:h:i:[j] | a <- [0..9],
                                 b <- [0..9],
                                 c <- [0..9],
                                 d <- [0..9],
                                 e <- [0..9],
                                 f <- [0..9],
                                 g <- [0..9],
                                 h <- [0..9],
                                 i <- [0..9],
                                 a * digit1 < 1000000,
                                 (a + 1) * digit1 > 1000000,
                                 let rest1 = 1000000 - (a * digit1),
                                 b * digit2 < rest1,
                                 (b + 1) * digit2 > rest1,
                                 let rest2 = rest1 - (b * digit2),
                                 c * digit3 < rest2,
                                 (c + 1) * digit3 > rest2,
                                 let rest3 = rest2 - (c * digit3),
                                 d * digit4 < rest3,
                                 (d + 1) * digit4 > rest3,
                                 let rest4 = rest3 - (d * digit4),
                                 e * digit5 < rest4,
                                 (e + 1) * digit5 > rest4,
                                 let rest5 = rest4 - (e * digit5),
                                 f * digit6 < rest5,
                                 (f + 1) * digit6 > rest5,
                                 let rest6 = rest5 - (f * digit6),
                                 g * digit7 < rest6,
                                 (g + 1) * digit7 > rest6,
                                 let rest7 = rest6 - (g * digit7),
                                 h * digit8 < rest7,
                                 (h + 1) * digit8 > rest7,
                                 let rest8 = rest7 - (h * digit8),
                                 i * digit9 < rest8,
                                 (i + 1) * digit9 > rest8,
                                 let j = rest8 - (i * digit9) -1]

-- We can do this 2 times -> first digit is 2
-- We can do this 2 times -> first digit is 2
digit1 = fac 9
digit2 = fac 8
digit3 = fac 7
digit4 = fac 6
digit5 = fac 5
digit6 = fac 4
digit7 = fac 3
digit8 = fac 2
digit9 = fac 1

-- Real solution 

main = print . toNumber . findDigits [0..9] $ (findIndices' [9,8..1] 1000000)

toNumber :: [Int] -> Int
toNumber = foldl1 (\ a b -> 10 * a + b) 

findDigits :: [Int] -> [Int] -> [Int]
findDigits _ [] = []
findDigits nbs (x:xs) = digit : findDigits newList xs
    where digit = nbs !! x
          newList = delete digit nbs 

findIndices' :: [Int] -> Int -> [Int] 
findIndices' [] l = [l-1] 
findIndices' (x:xs) limit = digit : (findIndices' xs newLimit)
    where digit = findIndex' (fac x) limit
          newLimit = limit - (digit * (fac x))

-- Factor -- Limit -- Result 
findIndex' :: Int -> Int -> Int 
findIndex' f m = head [n | n <- [0..9],
                           n * f < m, 
                           (n + 1) * f >= m]

fac :: Int -> Int 
fac nb = product [1..nb]
