-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms. 

main = do
         print . sum . filter (\ a -> mod a 2 == 0) . createList 4000000 $ [1,2]

createList :: Int -> [Int] -> [Int]
createList max list
    | next > max = list
    | otherwise = createList max (list ++ [next])
    where last' = last list
          len = length list
          butLast = list!!(len - 2)
          next = last' + butLast
