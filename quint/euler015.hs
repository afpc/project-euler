-- How many such routes are there through a 20×20 grid?
-- 02x02 -> X ~ B(04,02)
-- 20x20 -> X ~ B(40,20)

{- 1. There are only m steps to the right (R), and n to the down (D).
 - steps = m + n
 - 2. We can only have 20 R's and D's, so it is enough to know how many
 -    different positions R/D can take. (n k) = n!/(k!(n-k)!)
 -}
binomial :: Integer -> Integer -> Integer
binomial = b' 1 1
  where
    b' x y _ 0 = div x y
    b' _ _ 0 _ = 0
    b' x y n k = b' (x * n) (y * k) (n-1) (k-1)

{-
 - OPTIONAL
 - 3. (2n 2) = sum(i:1..n)(n+i)/i
 -}
-- loop :: Integer -> Integer
-- loop n = product [div (n+i) i | i <- [1..n]]      

main :: IO () 
main = print (binomial 40 20)

