-- By considering the terms in the Fibonacci sequence whose values do not exceed 
-- four million, find the sum of the even-valued terms.

import Data.List
import Data.Stream

streamWhile : (a -> Bool) -> Stream a -> List a
streamWhile f (x::xs) = 
  if   f x 
  then x :: (streamWhile f xs) 
  else []

even : Nat -> Bool
even Z     = True
even (S n) = not (even n)

fib : Stream Nat -- Why is this SOOOO slow?!
fib = 0 :: zipWith (+) fib (1::fib)

main : IO ()
main = print (sum (filter even (streamWhile (\x => x <= 4000000) fib)))

