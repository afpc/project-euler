{-
Combinatoric selections

There are exactly ten ways of selecting three from five, 12345:
123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation, C_53 = 10.

In general, C_nr = n! / (r!(n-r)!) where r <= n, n! = n * (n-1) * ... * 1, and 0! = 1.

It is not until n = 23, that a value exceeds one-million: C_23_10 = 1144066.

How many, not necessarily distinct, values of C_nr for 1 <= n <= 100, are greater than one million?   
-}

main = print . length $ generateValues

generateValues :: [Integer]
generateValues = [nb | n <- [1..100], 
                       r <- [1..n],
                       let nb = bin n r,
                       nb > 1000000]

bin :: Integer -> Integer -> Integer
bin n r  = div nom denom
    where nom = fac n 
          denom = (fac r) * (fac (n-r))

fac :: Integer -> Integer 
fac nb
    | nb == 0 = 1
    | otherwise = product [1..nb]
