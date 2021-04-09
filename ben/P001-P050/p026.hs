{-
A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1
Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
-}

import Data.Ord
import Data.List

main = print . fst . maximumBy (comparing snd) . map (\ x -> (x, periodLength 1 x [])) $ [1..1000]

fractions :: [Double]
fractions = [1/d | d <- [1..1000]]

fraction :: Int -> Int -> String
fraction n d
    | n == 0 = []
    | otherwise = (show (div n d)) ++ fraction ((mod n d)*10) d

-- actually not entirely correct, it checks the length until the period starts for a second time, the numbers before the period are included, but this works in this case. 
periodLength :: Int -> Int -> [Int] -> Int
periodLength n d list
    | r == 0 = 0
    | elem r list = length list
    | otherwise = periodLength (r*10) d (r:list)
        where r = mod n d
              q = div n d
