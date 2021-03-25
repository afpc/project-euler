-- What is the 10001st prime number?

primes :: [Int]
primes = sieve [2..] where
  sieve (p:xs) = p : sieve (filter (\x -> (mod x p) > 0) xs)

main :: IO ()
main = print (last (take 10001 primes))

