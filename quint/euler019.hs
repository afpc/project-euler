-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

-- The Julian day is the continuous count of days since the beginning of the 
-- Julian period, and is used primarily by astronomers, and in software for 
-- easily calculating elapsed days between two events.

-- toModifiedJulianDay (fromGregorian 1901 01 01) -> 15385 (Tuesday)
-- toModifiedJulianDay (fromGregorian 2000 12 31) -> 51909

import Data.Time.Calendar

firstDay :: Integer -> Bool
firstDay jd = (d == 1)
    where (y, m, d) = toGregorian (ModifiedJulianDay jd)

sunday :: Integer -> Bool
sunday jd = mod jd 7 == 6

firstSunday :: Integer -> Bool
firstSunday jd = (firstDay jd) && (sunday jd)

main :: IO ()
main = print (length (filter firstSunday [15385..51909]))

