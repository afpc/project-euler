-- What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

fib = 1 : 1 : zipWith (+) fib (tail fib)

main :: IO ()
main = print (snd (head (dropWhile (\(n,i) -> (length (show n)) < 1000) (zip fib [1..]))))

