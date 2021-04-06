{-
The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
-}

main = print . snd . simplify . foldr (\ (a,b) (c,d) -> (a*c,b*d)) (1,1) $ generateFractions

generateFractions :: [(Int, Int)]
generateFractions = [(a,b) | b0 <- [10..99],
                             a0 <- [10..(b0-1)],
                             let b1 = div b0 10,
                             let b2 = mod b0 10,
                             let a1 = div a0 10,
                             let a2 = mod a0 10,
                             b2 /= 0,
                             let (a,b) = simplify (a0,b0),
                             if a1 == b1 
                                 then simplify (a2,b2) == (a,b)
                             else if a1 == b2 
                                 then simplify (a2,b1) == (a,b)
                             else if a2 == b1 
                                 then simplify (a1,b2) == (a,b)
                             else if a2 == b2 
                                 then simplify (a1,b1) == (a,b)
                             else False]

simplify :: (Int,Int) -> (Int,Int)
simplify (a,b) = (div a d, div b d)
    where d = gcd a b
