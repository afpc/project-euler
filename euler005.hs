-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

main :: IO ()
main = print (foldr lcm 1 [2..20])

