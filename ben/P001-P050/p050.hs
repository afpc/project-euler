{-
The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13
This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
-}

-- very, very inefficient
main = print . fst . foldr (\(a1,b1) (a2,b2) -> if b1 > b2 
                                                   then (a1,b1) 
                                                else (a2,b2)) (0,0) $ consecutivePrimes

limit :: Int 
limit = 1000000

consecutivePrimes :: [(Int,Int)]
consecutivePrimes = let p = littlePrimes 
                    in [((sum list),(length list)) | d <- [0..(length p)],
                                                     t <- [1..(length p)-d],
                                                     let list = take t (drop d p),
                                                     let s = sum list,
                                                     s < limit,
                                                     elem (sum list) bigPrimes]  

littlePrimes :: [Int]
littlePrimes = takeWhile (<10000) primes

bigPrimes :: [Int]
bigPrimes = dropWhile (<100000) primes

primes :: [Int]
primes = sieve limit $ [2..limit]

sieve :: Int -> [Int] -> [Int]
sieve _ [] = []
sieve l (x:xs)
    | fromIntegral x < sqrt (fromIntegral l) = x : sieve l (removeMultiples (x:xs))
    | otherwise = (x:xs)

removeMultiples :: [Int] -> [Int]
removeMultiples list = filter (\ a -> mod a (head list) /= 0) list
