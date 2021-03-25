{-
The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)
-}

main = print . sum . takeWhile (<1000000) $ generateNb

generateNb :: [Int]
generateNb = [x | x <- [1..],
                  (show x) == (reverse . show $ x), 
                  let base2 = base10to2 x
                        in base2 == reverse base2]

base10to2 :: Int -> String
base10to2 0 = ""
base10to2 nb = base10to2 quot ++ show rem
    where (quot, rem) = quotRem nb 2
