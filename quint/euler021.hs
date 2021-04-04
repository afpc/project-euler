-- Evaluate the sum of all the amicable numbers under 10000.

d :: Int -> Int
d n = sum (filter (\x -> mod n x == 0) [1..(n-1)])

amicable :: Int -> [Int]
amicable n = [ 
  x |
    x <- [1..n],
    y <- [d x],
    x /= y,
    x == d y
  ]

main :: IO ()
main = print (sum (amicable 10000))
