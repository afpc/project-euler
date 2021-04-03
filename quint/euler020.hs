-- Find the sum of the digits in the number 100!

main :: IO ()
main = print (sum (map (\x -> read [x]) (show (product [1..100]))))

