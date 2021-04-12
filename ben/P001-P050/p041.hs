{-
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?
-}

import Data.List

main = print . head . dropWhile (not . isPrime) . reverse . sort $ panDigitalNbs

-- We only take n = 7 or n = 4, because we know there exists a 4-digit pandigital that is prime, and there could be a 7-digit pandigital that is prime. n = 5,6,8 or 9 could not be prime because all those numbers are divisible by 3. (Sum of digits is divisible by 3).
panDigitalNbs :: [Int]
panDigitalNbs = map (read) (permutations "1234567" ++ permutations "1234")

isPrime :: Int -> Bool 
isPrime nb = length primes == 0
    where root = floor . sqrt . fromIntegral $ nb
          k1 = [6*k+1 | k <- [1..root]]
          k2 = [6*k-1 | k <- [1..root]]
          f = [2,3] ++ k1 ++ k2
          primes = [x | x <- f,
                        x /= nb,
                        mod nb x == 0]
