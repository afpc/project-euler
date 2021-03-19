-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum. 

-- naive solution
main = print ((squareOfSums 100) - (sumOfSquares 100))

sumOfSquares :: Int -> Int
sumOfSquares nb = sum . map (\ a -> a^2) $ [1..nb]

squareOfSums :: Int -> Int
squareOfSums nb = (\ a -> a^2) (sum [1..nb])
