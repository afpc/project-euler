{-
Euler discovered the remarkable quadratic formula:

n^2 + n + 41

It turns out that the formula will produce 40 primes for the consecutive integer values 0≤n≤39. However, when n=40,40^2+40+41=40(40+1)+41 is divisible by 41, and certainly when n=41,41^2+41+41 is clearly divisible by 41.

The incredible formula n^2−79n+1601 was discovered, which produces 80 primes for the consecutive values 0≤n≤79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n^2+an+b, where |a|<1000 and |b|≤1000
where |n| is the modulus/absolute value of n
e.g. |11|=11 and |−4|=4

Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n=0.
-}

import Data.Ord
import Data.List
import Data.Numbers.Primes

main = print . (\ (a,b,_) -> a * b) . maximumBy (comparing trd) $ checkAllNumbers

-- b has to be prime since 0^2 + a0 + b has to be prime.
checkAllNumbers :: [(Int,Int,Int)]
checkAllNumbers = let p = takeWhile (<1000) primes
                  in [(a,b,l) | a <- [-999..999],
                                b <- p,
                                let l = length (generatePrimeList a b),
                                l > 40]

-- According to formula n^2+an+b
generatePrimeList :: Int -> Int -> [Int]
generatePrimeList a b = takeWhile (isPrime) (map (\ n -> n^2 + a*n + b) [0..])

-- Help functions 

trd :: (a,b,c) -> c
trd (_,_,c) = c
