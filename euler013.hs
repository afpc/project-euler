-- Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.

main :: IO ()
main = do
  input <- readFile "input/p013.in"
  putStrLn (take 10 (show (sum (map read (lines input)))))

