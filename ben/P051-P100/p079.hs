{-
Passcode derivation.

A common security method used for online banking is to ask the user for three random characters from a passcode. For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.

The text file, keylog.txt, contains fifty successful login attempts.

Given that the three characters are always asked for in order, analyse the file so as to determine the shortest possible secret passcode of unknown length.
-}

import Data.List

main = do input <- readFile "../../input/p079_keylog.txt"
          print . output . nub' . order createEmptyData . parse $ input

parse :: String -> [[Int]]
parse = map (map (\ x -> read [x])) . lines

data NumberData = Nb [Int] Int [Int]
    deriving Show

getNb :: NumberData -> Int
getNb (Nb _ nb _) = nb

createEmptyData :: [NumberData]
createEmptyData = [(Nb [] nb []) | nb <- [0..9]]

order :: [NumberData] -> [[Int]] -> [NumberData]
order d [] = d
order d (x:xs) = order third xs 
    where first = add (x!!0) (x!!1) d
          second = add (x!!0) (x!!2) first
          third = add (x!!1) (x!!2) second

add :: Int -> Int -> [NumberData] -> [NumberData] 
add n1 n2 d = d2
    where (b, ((Nb b1 nb1 a1):a)) = splitAt (n1) d
          d1 = b ++ ((Nb b1 nb1 (n2:a1)):a)
          (b', ((Nb b2 nb2 a2):a')) = splitAt (n2) d1
          d2 = b' ++ ((Nb (n1:b2) nb2 a2):a')

nub' :: [NumberData] -> [NumberData]
nub' [] = []
nub' ((Nb b nb a):xs) 
    | length a' == 0 && length b' == 0 = nub' xs
    | otherwise = (Nb b' nb a') : nub' xs
    where a' = nub a
          b' = nub b

output :: [NumberData] -> String
output [] = []
output list = (show nb) ++ output newList
    where sorted = sortBy ord list
          nb = getNb (head sorted)
          newList = deleteNb nb sorted

ord :: NumberData -> NumberData -> Ordering
ord (Nb b1 _ _) (Nb b2 _ _)
    | (length b1) > (length b2) = GT 
    | (length b1) == (length b2) = EQ 
    | otherwise = LT

deleteNb :: Int -> [NumberData] -> [NumberData]
deleteNb _ [] = []
deleteNb nb ((Nb b n a):xs) 
    | nb == n = deleteNb nb xs
    | otherwise = (Nb (delete nb b) n a) : deleteNb nb xs
