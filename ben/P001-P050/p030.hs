{- Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

1634 = 14 + 64 + 34 + 44
8208 = 84 + 24 + 04 + 84
9474 = 94 + 44 + 74 + 44
As 1 = 14 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

-}

main = print . sum $ fifthPowerDigits

-- Maybe the limit should be 6 * 9^5 not completely sure about that. 
limit :: Int
limit = 5 * 9^5

fifthPowerDigits :: [Int]
fifthPowerDigits = [x | x <- [10..limit], 
                        let digits = map (\ a -> read [a] :: Int) (show x)
                            in (sum . map (\ b -> b^5) $ digits) == x]
