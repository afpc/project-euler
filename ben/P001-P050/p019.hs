{-
You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
-}

jan = 31
feb = 28
feb2 = 29
mar = 31
apr = 30
may = 31
jun = 30
jul = 31
aug = 31
sep = 30
oct = 31
nov = 30
dec = 31

months = [jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec]
leap_months = [jan, feb2, mar, apr, may, jun, jul, aug, sep, oct, nov, dec]

-- Weekdays represented by numbers 0 = sunday, 1 = monday ...
startDay = 1
startYear = 1901
endYear = 2000

main = print . sum $ sundays

leapYear :: Int -> Bool 
leapYear year = (mod year 4 == 0) && (mod year 100 /= 0 || mod year 400 == 0)

yearLengths :: [Int] 
yearLengths = [days | year <- [1900..endYear],
                      let days = if leapYear year
                                     then 366
                                 else 365]

newYear :: Int -> Int
newYear year = mod (startDay + days) 7
    where days = sum . take (year - 1900) $ yearLengths

sundays :: [Int]
sundays = [nb | year <- [startYear..endYear],
                let nb = calculateSundays year]

calculateSundays :: Int -> Int
calculateSundays year = sun
    where m = if leapYear year
                  then leap_months
              else months
          start = newYear year
          first_days = scanl (\ a b -> mod (a + b) 7) start m
          sun = length . filter (\ a -> mod a 7 == 0) . init $ first_days
