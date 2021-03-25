-- By considering the terms in the Fibonacci sequence whose values do not exceed 
-- four million, find the sum of the even-valued terms.

fib = 0 : 1 : zipWith (+) fib (tail fib)

main :: IO ()
main = print (sum (takeWhile (< 4000000) (filter even fib)))

