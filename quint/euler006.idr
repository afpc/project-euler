-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

sumOfSquares : Int -> Int
sumOfSquares n = sum (map (\x => x*x) [1..n])

squareOfSums : Int -> Int
squareOfSums n = (\x => x*x) (sum [1..n])

main : IO ()
main = print ((squareOfSums 100) - (sumOfSquares 100))

