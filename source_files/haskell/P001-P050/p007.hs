-- What is the 10001st prime number?

main = print (((primes [2])!!10000)!!10000)
--main = print . take 100 $ (primes [2])

primes :: [Int] -> [[Int]]
primes start = iterate nextPrime start

nextPrime :: [Int] -> [Int]
nextPrime p = p ++ take 1 list
    where lastPrime = last p
          list = [next | next <- [(lastPrime + 1)..], 
                         checkPrime next p]

checkPrime :: Int -> [Int] -> Bool 
checkPrime nb [] = True
checkPrime nb (x:xs) 
    | mod nb x /= 0 = checkPrime nb xs
    | otherwise = False 
