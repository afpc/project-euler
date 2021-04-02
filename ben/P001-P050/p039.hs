{-
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?
-}

import Data.List
import Data.Ord

main = print . snd . maximumBy (comparing fst) . map (\ a -> (length a, head a)) . group $ findSolutions
--main = print . fst . maximumBy (comparing snd) . map findSolutions2 $ [1..1000]

-- very very inefficient
findSolutions :: [Int]
findSolutions = [ n | n <- [1..1000], 
                      c <- [1..n],
                      b <- [c..(n-c)],
                      let a = n-b-c,
                      a^2 == b^2 + c^2]

findSolutions2 :: Int -> (Int,Int)
findSolutions2 n = (n, length [n | a <- [1..n],
                                   b <- [1..(n-a)],
                                   let c = n-b-a,
                                   a^2 == b^2 + c^2])
