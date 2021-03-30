{-
It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

9 = 7 + 2×12
15 = 7 + 2×2^2
21 = 3 + 2×3^2
25 = 7 + 2×3^2
27 = 19 + 2×2^2
33 = 31 + 2×1^2

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
-}

main = print . head $ breakingNbs

breakingNbs :: [Integer]
breakingNbs = [x | x <- [1,3..],
                   not (isPrime x),
                   not (checkCondition x)]

checkCondition :: Integer -> Bool
checkCondition nb = length sol /= 0
    where sol = [(p,s) | s <- [1..l],
                         let p = nb - 2*s^2, 
                         isPrime p]
          l = floor . sqrt . fromIntegral . div nb $ 2

isPrime :: Integer -> Bool 
isPrime nb = length primes == 0
    where root = floor . sqrt . fromIntegral $ nb
          k1 = [6*k+1 | k <- [1..root]]
          k2 = [6*k-1 | k <- [1..root]]
          f = [2,3] ++ k1 ++ k2
          primes = [x | x <- f,
                        x /= nb,
                        mod nb x == 0]
