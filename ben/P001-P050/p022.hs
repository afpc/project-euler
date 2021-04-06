{-
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
-}

import Data.List
import Data.List.Split
import Data.Char 

main = do input <- readFile "../../input/p022_names.txt"
          print . sum . calculate . parse $ input

parse :: String -> [String]
parse = sort . map (init . tail) . splitOn ","

value :: Char -> Int
value char = ord char - ord 'A' + 1

calculate :: [String] -> [Int]
calculate input = [score | n <- [0..(length input -1)],
                           let score = (n+1) * (sum . map (value) $ (input!!n))]
