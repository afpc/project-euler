{-
In the 20×20 grid in file p011_matrix, four numbers along a specific diagonal line are 26, 63, 78, 14.

The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
-}

import Data.Matrix

main = do input <- readFile "../../input/p011_matrix.txt"
          let grid = map (\ x -> read x :: Int) (words input)
          let matrix = fromList 20 20 grid
                in print . maximum . findNumbers $ matrix

findNumbers :: Matrix Int -> [Int]
findNumbers matrix = [a*b*c*d | i <- [1..20],
                                j <- [1..17],
                                let a = getElem i j matrix,
                                let b = getElem i (j+1) matrix,
                                let c = getElem i (j+2) matrix,
                                let d = getElem i (j+3) matrix]
                     ++
                     [a*b*c*d | i <- [1..17],
                                j <- [1..20],
                                let a = getElem i j matrix,
                                let b = getElem (i+1) j matrix,
                                let c = getElem (i+2) j matrix,
                                let d = getElem (i+3) j matrix]
                     ++
                     [a*b*c*d | i <- [1..17],
                                j <- [1..17],
                                let a = getElem i j matrix,
                                let b = getElem (i+1) (j+1) matrix,
                                let c = getElem (i+2) (j+2) matrix,
                                let d = getElem (i+2) (j+3) matrix]
                     ++
                     [a*b*c*d | i <- [1..17],
                                j <- [4..20],
                                let a = getElem i j matrix,
                                let b = getElem (i+1) (j-1) matrix,
                                let c = getElem (i+2) (j-2) matrix,
                                let d = getElem (i+3) (j-3) matrix]
