{-
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?
-}

import Data.List
import Data.Ord

main = print . snd . maximumBy (comparing fst) . map (\ a -> (length a, head a)) . group $ findSolutions

findSolutions :: [Int]
findSolutions = [ n | n <- [1..1000], 
                      c <- [1..n],
                      b <- [c..(n-c)],
                      let a = n-b-c,
                      a > b, 
                      a > c,
                      a < b + c,
                      b < a + c,
                      c < a + b,
                      a^2 == b^2 + c^2]
