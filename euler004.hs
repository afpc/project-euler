-- Find the largest palindrome made from the product of two 3-digit numbers.

palindrome :: Int -> Bool
palindrome n = show n == reverse (show n) 

main :: IO ()
main = print (maximum ([x * y | x <- [100..999], y <- [100..999], palindrome (x * y)]))

