{-
Spiral primes.

Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?
-}

import Data.Numbers.Primes

main :: IO()
main = print (calculate 3 (0,1))

calculate :: Int -> (Int,Int) -> Int
calculate nb (rn, rd) 
    | (fromIntegral rn'') / (fromIntegral rd'') < 0.1 = nb
    | otherwise = calculate (nb + 2) ratio
    where newNbs = oneRing nb
          (rn', rd') = primeRatio newNbs 
          ratio@(rn'', rd'') = (rn + rn', rd + rd')

primeRatio :: [Int] -> (Int,Int)
primeRatio nbs = (p,l)
    where l = length nbs
          p = length . filter (isPrime) $ nbs

-- If n is the size of the square,
-- the upper right corner is represented by n^2,
-- the upper left corner is represented by n^2-n+1,
-- the bottom left corner is represented by n^2-2n+2,
-- the bottom right corner is represented by n^2-3n+3.

oneRing :: Int -> [Int]
oneRing nb = a:b:c:[d]
    where a = nb^2
          b = nb^2 - nb + 1
          c = nb^2 - 2 * nb + 2
          d = nb^2 - 3 * nb + 3
