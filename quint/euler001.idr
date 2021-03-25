-- Find the sum of all the multiples of 3 or 5 below 1000.

isMultiple : Int -> Int -> Bool
isMultiple n m = (mod n m) == 0

main : IO ()
main = print (sum [x | x <- [1..999], isMultiple x 3 || isMultiple x 5])

