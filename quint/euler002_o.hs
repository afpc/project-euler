-- By considering the terms in the Fibonacci sequence whose values do not exceed 
-- four million, find the sum of the even-valued terms.

{- [1,1,2,3,5,8,13,21,34,55,89,144,...]
 - [o,o,e,o,o,e, o, o, e, o, o,  e,...]
 - Every third Fibonacci number is even.
 - F(n) = F(n-1) + F(n-2)                |
 - = F(n-2) + F(n-3) + F(n-2)            | F(n-1) = F(n-2) + F(n-3) 
 - = 2*F(n-2) + F(n-3)                   | 
 - = 2*(F(n-3) + F(n-4)) + F(n-3)        | F(n-2) = F(n-3) + F(n-4)
 - = 3*F(n-3) + 2*F(n-4)                 |
 - = 3*F(n-3) + F(n-4) + F(n-5) + F(n-6) | F(n-4) = F(n-5) + F(n-6)
 - = 4*F(n-3) + F(n-6)                   | F(n-3) = F(n-4) + F(n-5)
 -}

fib = 2 : 8 : zipWith (\a b -> a + 4*b) fib (tail fib)

main :: IO ()
main = print (sum (takeWhile (< 4000000) (filter even fib)))

