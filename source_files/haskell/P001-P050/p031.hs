-- In the United Kingdom the currency is made up of pound (£) and pence (p). There are eight coins in general circulation:
-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).
-- How many different ways can £2 be made using any number of coins?

main = print . length . calculateCoins $ 200

coins :: [Int]
coins = [200,100,50,20,10,5,2,1]

calculateCoins :: Int -> [[Int]]
calculateCoins nb = [[a,b,c,d,e,f,g,h] | h <- [0..(div nb 200)],
                                         g <- [0..(div (nb - (sum . zipWith (*) coins $ [h])) 100)],
                                         f <- [0..(div (nb - (sum . zipWith (*) coins $ [h,g])) 50)],
                                         e <- [0..(div (nb - (sum . zipWith (*) coins $ [h,g,f])) 20)],
                                         d <- [0..(div (nb - (sum . zipWith (*) coins $ [h,g,f,e])) 10)],
                                         c <- [0..(div (nb - (sum . zipWith (*) coins $ [h,g,f,e,d])) 5)],
                                         b <- [0..(div (nb - (sum . zipWith (*) coins $ [h,g,f,e,d,c])) 2)],
                                         a <- [0..(nb - (sum . zipWith (*) coins $ [h,g,f,e,d,c,b]))],
                                         (sum . zipWith (*) coins $ [h,g,f,e,d,c,b,a]) == nb] 
