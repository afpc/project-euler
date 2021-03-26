{-
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
-}

main = print . sum . amicableNbs $ 10000

amicableNbs :: Int -> [Int]
amicableNbs limit = [a | a <- [1..limit],
                             amicableNb a,
                             let b = amicable a,
                             a /= b]
amicable :: Int -> Int
amicable = sum . divisors

amicableNb :: Int -> Bool 
amicableNb a = (amicable b) == a
        where b = amicable a

divisors :: Int -> [Int] 
divisors nb = let limit = floor (sqrt (fromIntegral nb))
              in 1: concat [[x, div nb x] | x <- [2..limit], 
                                            mod nb x == 0]
