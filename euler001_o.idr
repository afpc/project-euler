-- Find the sum of all the multiples of 3 or 5 below 1000.

{- A more optimal solution would be to calulate the sum of numbers divisible by
 - 3 plus the numbers divisible by 5. This way you added multiples by 15 twice.
 - 
 - Example:
 - 3 +  6 +  9 + ... + 999 = 3 * (1 + 2 + 3 + ... + 333)
 - 5 + 10 + 15 + ... + 199 = 5 * (1 + 2 + 3 + ... + 199)
 - 1 +  2 +  3 + ... +   n = (n * (n + 1)) / 2
 - -> for more info search for 'triangular number'.
 -}
sum' : Int -> Int -> Int
sum' n m = div (n * t * (t + 1)) 2 where 
  t : Int
  t = div m n

main : IO ()
main = print ((sum' 3 999) + (sum' 5 999) - (sum' 15 999))

