module Data.DateTime (DateTime(), addYears, addMonths, addDays, addHours, addMinutes, toParts, fromParts) where
import Data.Function


data DateTime = DateTime Int Int Int Int Int Int Int deriving (Eq, Ord, Show)


fromParts :: ( Int, Int, Int, Int, Int, Int, Int ) -> DateTime
fromParts ( year, month, day, hour, min, sec, ms )
  | month >= 1 && month <= 12 && day >= 1 && day <= (maxDay year month) && hour >= 0 && hour <= 23 && min >= 0 && min <= 59 && sec >= 0 && sec <= 59 && ms >= 0 && ms <= 999 = DateTime year month day hour min sec ms
  | otherwise = error "Invalid date construction"


toParts :: DateTime -> ( Int, Int, Int, Int, Int, Int, Int )
toParts (DateTime year month day hour min sec ms) = ( year, month, day, hour, min, sec, ms )


addYears :: Int -> DateTime -> DateTime
addYears 0 dt = dt
addYears amount (DateTime year month day hour min sec ms) = (DateTime (year + amount) month day hour min sec ms)


addMonths :: Int -> DateTime -> DateTime
addMonths amount dt@(DateTime year month day hour min sec ms)
  | amount == 0 = dt
  | amount < 0 && month == 1 = (DateTime year 12 day hour min sec ms) & addYears (-1) & addMonths (amount + 1) 
  | amount < 0 = (DateTime year (month - 1) day hour min sec ms) & addMonths (amount + 1)
  | month == 12 = (DateTime year 1 day hour min sec ms) & addYears 1 & addMonths (amount - 1)
  | otherwise = (DateTime year (month + 1) day hour min sec ms) & addMonths (amount - 1)


addDays :: Int -> DateTime -> DateTime
addDays amount dt@(DateTime year month day hour min sec ms)
  | amount == 0 = dt
  | amount < 0 && day == 1 = dt & addMonths (-1) & toMaxDay & addDays (amount + 1)
  | amount < 0 = (DateTime year month (day - 1) hour min sec ms) & addDays (amount + 1)
  | day == maxDay year month = (DateTime year month 1 hour min sec ms) & addMonths 1 & addDays (amount - 1)
  | otherwise = (DateTime year month (day + 1) hour min sec ms) & addDays (amount - 1)


addHours :: Int -> DateTime -> DateTime
addHours amount dt@(DateTime year month day hour min sec ms)
  | amount == 0 = dt
  | amount < 0 && hour == 0 = (DateTime year month day 23 min sec ms) & addDays (-1) & addHours (amount + 1)
  | amount < 0 = (DateTime year month day (hour - 1) min sec ms) & addHours (amount + 1)
  | hour == 23 = (DateTime year month day 0 min sec ms) & addDays 1 & addHours (amount - 1)
  | otherwise = (DateTime year month day (hour + 1) min sec ms) & addHours (amount - 1)


addMinutes :: Int -> DateTime -> DateTime
addMinutes amount dt@(DateTime year month day hour min sec ms)
  | amount == 0 = dt
  | amount < 0 && min == 0 = (DateTime year month day hour 59 sec ms) & addHours (-1) & addMinutes (amount + 1)
  | amount < 0 = (DateTime year month day hour (min - 1) sec ms) & addMinutes (amount + 1)
  | min == 59 = (DateTime year month day hour 0 sec ms) & addHours 1 & addMinutes (amount - 1)
  | otherwise = (DateTime year month day hour (min + 1) sec ms) & addMinutes (amount - 1)


isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy _ 0 = False
isDivisibleBy num denom =
  mod num denom == 0


isLeapYear :: Int -> Bool
isLeapYear year =
  isDivisibleBy year 400 || (not (isDivisibleBy year 100) && isDivisibleBy year 4)


maxDay :: Int -> Int -> Int
maxDay year month
  | month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 = 31
  | month == 4 || month == 6 || month == 9 || month == 11 = 30
  | month == 2 && isLeapYear year = 29
  | month == 2 = 28
  | otherwise = error "Invalid month"


toMaxDay :: DateTime -> DateTime
toMaxDay (DateTime year month day hour min sec ms) =
  DateTime year month (maxDay year month) hour min sec ms
