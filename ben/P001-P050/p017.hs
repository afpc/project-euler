{-
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
-}

main = print . sum $ [10 * oneTo99, hundreds, thousand]

oneTo99 :: Int
oneTo99 = lastDigit + teens + prefix
    where lastDigit = 9 * (sum . map (length) $ zeroDigits)
          teens = sum . map (length) $ specials
          prefix = 10 * (sum . map (length) $ oneDigits)

hundreds :: Int 
hundreds = 9 * length hundred + 891 * length hundredAnd + 100 * (sum . map (length) $ zeroDigits)

thousand :: Int
thousand = length ("One" ++ "Thousand")

zeroDigits :: [String]
zeroDigits = ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]

oneDigits :: [String]
oneDigits = ["Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"]

hundred :: String
hundred = "Hundred"

hundredAnd :: String
hundredAnd = "Hundred" ++ "And"

specials :: [String]
specials = ["Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"]
