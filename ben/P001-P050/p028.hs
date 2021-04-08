{-
Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
-}

limit :: Int
limit = 1001

main :: IO()
-- main = print . sum . next 2 $ [1] 
main = print . sum $ calculate

next :: Int -> [Int] -> [Int]
next diff list
    | diff > limit = list
    | otherwise = next (diff + 2) (list ++ newList)
        where newList = tail . take 5 .iterate (diff+) . last $ list

-- Even smarter: 
-- If n is the size of the square,
-- the upper right corner is represented by n^2,
-- the upper left corner is represented by n^2-n+1,
-- the bottom left corner is represented by n^2-2n+2,
-- the bottom right corner is represented by n^2-3n+3.
-- Summed up this equals 4n^2-6n+5

calculate :: [Int]
calculate = 1 : [4*n^2-6*n+6 | n <- [3,5..limit]]
