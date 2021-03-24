{-
 The series, 11 + 22 + 33 + ... + 1010 = 10405071317.

 Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.
 -}

-- Brute force

main = print . reverse . take 10 . reverse . show $ number

number :: Integer 
number = sum [x^x | x <- [1..1000]]
