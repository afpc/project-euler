-- Find the sum of all the primes below two million.

-- -> Sieve of eratosthenes
main = print . sum . sieve 2000000 $ [2..2000000]

sieve :: Int -> [Int] -> [Int]
sieve _ [] = []
sieve limit (x:xs) 
    | fromIntegral x < sqrt (fromIntegral limit) = x : sieve limit (removeMultiples (x:xs))
    | otherwise = (x:xs)

removeMultiples :: [Int] -> [Int]
removeMultiples list = filter (\ a -> mod a (head list) /= 0) list
