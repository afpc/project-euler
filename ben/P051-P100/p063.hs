{-
Powerful digit counts.

The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.

How many n-digit positive integers exist which are also an nth power?
-}

main = print . length $ powers

-- All powers with 10 as base have 1 more digit than the size of the exponent. 
baseLimit :: Integer
baseLimit = 9

exponentLimit :: Int 
exponentLimit = head [n | n <- [1..],
                          let s = 9^n,
                          (length . show $ s) < n]

powers :: [Int]
powers = concat [p | n <- [1..exponentLimit],
                     let p = nthPowers n]

nthPowers :: Int -> [Int]
nthPowers nb = filter (== nb) . takeWhile (<= nb) $ [length (show (n^nb)) | n <- [1..baseLimit]]   
