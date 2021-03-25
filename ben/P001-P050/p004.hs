-- Find the largest palindrome made from the product of two 3-digit number. 

main = print . maximum $ findPalindrome

-- naive solution
findPalindrome :: [Int]
findPalindrome = [a * b | a <- [100..999],
                          b <- [a..999], 
                          (reverse . show $ (a * b)) == show (a * b)]
