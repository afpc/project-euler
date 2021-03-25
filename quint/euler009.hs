-- There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.

triplet :: Int -> [Int]
triplet n = [a*b*c |
              a <- [1..n],      -- a = max n
              b <- [1..(n-a)],  -- b = max n - a
              c <- [n - a - b], -- c = max n - a - b
              a + b + c == n,
              a*a + b*b == c*c
            ]

main :: IO ()
main = print (head (triplet 1000))

