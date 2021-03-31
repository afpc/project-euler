-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

toWord :: Int -> String
toWord 0    = "zero"
toWord 1    = "one"
toWord 2    = "two"
toWord 3    = "three"
toWord 4    = "four"
toWord 5    = "five"
toWord 6    = "six"
toWord 7    = "seven"
toWord 8    = "eight"
toWord 9    = "nine"
toWord 10   = "ten"
toWord 11   = "eleven"
toWord 12   = "twelve"
toWord 13   = "thirteen"
toWord 14   = "fourteen"
toWord 15   = "fifteen"
toWord 16   = "sixteen"
toWord 17   = "seventeen"
toWord 18   = "eighteen"
toWord 19   = "nineteen"
toWord 20   = "twenty"
toWord 30   = "thirty"
toWord 40   = "forty"
toWord 50   = "fifty"
toWord 60   = "sixty"
toWord 70   = "seventy"
toWord 80   = "eighty"
toWord 90   = "ninety"
toWord 1000 = "one thousand"
toWord n
 | (mod n 100) == 0 = (toWord (div n 100)) ++ " hundred"
 | n < 100          = (toWord (n - last))  ++ " "     ++ (toWord last)
 | otherwise        = (toWord (n - hnrd))  ++ " and " ++ (toWord hnrd)
 where
  last = mod n 10
  hnrd = mod n 100

countLetters s = sum (map length (words (toWord s)))

main :: IO ()
main = print (sum (map countLetters [1..1000]))

