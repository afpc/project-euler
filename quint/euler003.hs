-- What is the largest prime factor of the number 600851475143?

maxFactor :: Integer -> Integer
maxFactor n = factor n 2 where
  div'   n m = until (\x -> (mod x m) /= 0) (`div` m) n  -- div as much as possible.
  factor n m 
    | n < m * m      = n                                 -- m is too large
    | (mod n m) == 0 = max m (factor (div' n m) (m + 1)) -- n is divisible by m.
    | otherwise      = factor n (m + 1)                  -- m++

main :: IO ()
main = print (maxFactor 600851475143)

