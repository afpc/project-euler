-- What is the value of the first triangle number to have over five hundred divisors?

{- t = n(n+1)/2
 - n and n+1 are coprime and thus have no common divisor.
 - d(t) = d(n/2) * d(n+1)  | n is even
 - d(t)_= d(n)   * d(n+1n) | n is odd
 -}
numberOfDivisors :: Int -> Int
numberOfDivisors n  
  | even n = (number (n+1)) * (number (div n 2))
  | odd  n = (number n    ) * (number (div (n+1) 2))
  where
    number n = length (filter (\d -> mod n d == 0) [1..n])

main :: IO ()
main = print (div (n * (n + 1)) 2) -- sum of [1..n]
    where n = head (dropWhile (\x -> (numberOfDivisors x) <= 500) [1..])


