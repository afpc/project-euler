-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

import Data.List (unfoldr, maximumBy)
import Data.Ord (comparing)

cycleLen :: Int -> Int
cycleLen n = 1 + length (unfoldr c (mod 10 n)) where
  c 1 = Nothing
  c x = Just (x, mod (x * 10) n)

main :: IO ()
main = print (fst (maximumBy (comparing snd) (zip xs (map cycleLen xs))))
  where xs = filter (\x -> mod x 2 /= 0 && mod x 5 /= 0) [3..999]

