-- Find the sum of the digits in the number 100!

main = sum . map (\ x -> read [x] :: Int) . show . product $ [1..100]
