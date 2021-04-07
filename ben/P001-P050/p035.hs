{-
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
-}

import Data.List

main = print . length $ getNumbers

getNumbers :: [Int]
getNumbers = 2:5: [nb | nb <- primes,
                        let s = show nb,
                        not (elem '2' s),
                        not (elem '4' s),
                        not (elem '6' s),
                        not (elem '8' s),
                        not (elem '5' s),
                        check s]

check :: String -> Bool
check nb = and . map (\ x -> elem (read x) primes) $ rot
    where rot = rotations nb nb
                   
rotations :: String -> String -> [String]
rotations ori (x:xs)
    | ori == new = [ori] 
    | otherwise = new : (rotations ori new)
    where new = xs ++ [x]

primes :: [Int]
primes = sieve 1000000 [2..1000000]

sieve :: Int -> [Int] -> [Int]
sieve _ [] = []
sieve limit (x:xs)
    | fromIntegral x < sqrt (fromIntegral limit) = x : sieve limit (removeMultiples (x:xs))
    | otherwise = (x:xs)

removeMultiples :: [Int] -> [Int]
removeMultiples list = filter (\ a -> mod a (head list) /= 0) list
