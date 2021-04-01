-- Find the maximum total from top to bottom of a triangle. (see input/p018.in)

parseTriangle :: String -> [[Int]]
parseTriangle s = map (map read) (map words (lines s))

-- example: reduce [1,2] [3,4,5] -> [5,7]
reduce :: [Int] -> [Int] -> [Int]
reduce u l = zipWith max u' l' where
  u' = zipWith (+) u l
  l' = zipWith (+) u (tail l)

main :: IO ()
main = do
  input <- readFile "input/p018.in"
  print (head (foldr1 reduce (parseTriangle input)))

