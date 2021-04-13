{-
Square root convergents.

It is possible to show that the square root of two can be expressed as an infinite continued fraction.

\sqrt 2 = 1+ \frac 1 {2+ \frac 1 {2 +\frac 1 {2+ ...}}}

By expanding this for the first four iterations, we get:
1 + 1/2 = 3/2 = 1.5
1 + 1 / (2 + (1/2)) = 7/5 = 1.4
1 + 1 / (2 + (1 / (2 + (1/2)))) = 17/12 = 1.41666...
1 + 1 / (2 + (1 / (2 + (1 / (2 + / (1/2)))))) = 41/29 = 1.41379...

The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.

In the first one-thousand expansions, how many fractions contain a numerator with more digits than the denominator?
-}

import Data.Ratio

-- brute force

-- main = print . length . filter (\ (a,b) -> length (show a) > length (show b)) $ generateExpansions

generateExpansions :: [(Integer,Integer)]
generateExpansions = [ (numerator ex, denominator ex) | n <- [1..1000],
                                                        let ex = expansion n]

-- input is recursion depth. 
expansion :: Int -> Rational 
expansion depth = (1 % 1) + (1 / (fraction depth))

fraction :: Int -> Rational
fraction 1 = (2 % 1)
fraction n = (2 % 1) + (1 / fraction (n-1))

-- somewhat smarter

main = print . length . select . take 1000 $ generateExpansions' 

select :: [Rational] -> [Rational]
select = filter (\ a -> length (show (numerator a)) > length (show (denominator a)))

generateExpansions' :: [Rational]
generateExpansions' = map (+1) . iterate expansion' $ (1 % 2)

expansion' :: Rational -> Rational
expansion' nb = 1 / ((2 % 1) + nb)
