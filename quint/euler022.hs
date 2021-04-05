-- What is the total of all the name scores in the file?

import Data.Char
import Data.List
import Data.List.Split

parse :: String -> [String]
parse i = map (\(x:xs) -> init xs) (splitOn "," i)

score :: String -> Int
score n = foldl (\n c -> n + (ord c - ord 'A' + 1)) 0 n

solve :: String -> Int
solve i = sum (map (\(n, i) -> (score n) * i) pairs)
  where
    names = sort (parse i)
    pairs = zip names [1..]

main :: IO ()
main = do
  input <- readFile "input/p022.in"
  print (solve (head (lines input)))

