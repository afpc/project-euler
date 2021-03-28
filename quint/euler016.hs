-- What is the sum of the digits of the number 21000?

sumOfDigit :: Integer -> Integer
sumOfDigit 0 = 0
sumOfDigit n = (mod n 10) + (sumOfDigit (div n 10))

main = print (sumOfDigit (2^1000))

