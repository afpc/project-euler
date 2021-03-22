-- Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?

minus x'@(x:xs) y'@(y:ys) = case (compare x y) of
  LT -> x : minus xs y'
  EQ ->     minus xs ys
  GT ->     minus x' ys
minus xs        _         = xs

primesTo :: Int -> [Int]
primesTo n = 2 : sieve [3,5..n] where
  sieve (p:xs)
    | p*p > n   = p : xs
    | otherwise = p : sieve (minus xs (map (p*) [p,p+2..]))

main :: IO ()
main = print (sum (primesTo 2000000))


