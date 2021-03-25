-- There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product a b c.
-- A Pythogorean triplet is a set of three natural numbers, a < b < c, for which a^2 + b^2 = c^2. 

main = print . head . pythTriplet $ 1000

pythTriplet :: Int -> [Int]
pythTriplet nb = [a * b * c | c <- [1..nb], 
                              b <- [1..(nb-c)],
                              a <- [nb-c-b],
                              a + b + c == nb,
                              a^2 +b^2 == c^2]
