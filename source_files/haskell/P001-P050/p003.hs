-- What is the largest prime factor of the number 600851475143?

main = do 
          print . maximum . primeFactorisation $ 600851475143

-- Dumb method 
findSmallestDivisor :: Int -> Int -> Int
findSmallestDivisor start nb 
    | mod nb start == 0 = start
    | otherwise = findSmallestDivisor (start + 1) nb

primeFactorisation :: Int -> [Int]
primeFactorisation nb 
    | smallest == nb = [smallest]
    | otherwise = smallest : (primeFactorisation (div nb smallest)) 
    where smallest = findSmallestDivisor 2 nb
