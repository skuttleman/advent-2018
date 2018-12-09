module Day4 (step1, step2) where
import Data.DateTime
import Data.Function
import Data.List
import qualified Data.Map as Map
import Utils.Lists
import Utils.Maps
import Utils.Regex
import Utils.Tuples


data GuardEvent = BeginShift Int DateTime | FallAsleep DateTime | WakeUp DateTime


step1 :: [String] -> String
step1 = step' mostMinutesSlept


step2 :: [String] -> String
step2 = step' largestSleepingMinute


step' :: (( Int, Map.Map Int Int ) -> ( Int, Map.Map Int Int ) -> Ordering) -> [String] -> String
step' sorter lines =
  lines
    & sort
    & fmap toGuardEvent
    & foldPairsL trackSleep ( Nothing, Map.empty )
    & snd
    & Map.toList
    & sortBy sorter
    & head
    & map2of2 (head . sortBy mostSleptMinute . Map.toList)
    & idByMinute
    & show


toGuardEvent :: String -> GuardEvent
toGuardEvent s =
  let
    (dt:event:_) = (s #= "\\[(.+)\\] (.+)")
    (year:month:day:hour:min:_) = (fmap read (dt #= "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})"))
    constructor = toGuardEventConstructor event
  in
    constructor (fromParts ( year, month, day, hour, min, 0, 0 ))


toGuardEventConstructor :: String -> DateTime -> GuardEvent
toGuardEventConstructor s =
  case s of
    "falls asleep" -> FallAsleep
    "wakes up" -> WakeUp
    _ -> s #= "Guard #([0-9]+) begins shift" & head & read & BeginShift


trackSleep :: ( Maybe Int, Map.Map Int (Map.Map Int Int) ) -> GuardEvent -> GuardEvent -> ( Maybe Int, Map.Map Int (Map.Map Int Int) )
trackSleep ( _, m ) event1@(BeginShift id _) _ = ( Just id, m )
trackSleep ( Just id, m ) event1@(FallAsleep datetime) event2 =
  ( Just id, updateWith Map.empty (incTimes (expand datetime (toDateTime event2))) id m )
trackSleep m _ _ = m


incTimes :: [DateTime] -> Map.Map Int Int -> Map.Map Int Int
incTimes [] m = m
incTimes (dt:dts) m =
  updateWith 0 (+ 1) (toMin dt) m
    & incTimes dts


toDateTime :: GuardEvent -> DateTime
toDateTime (BeginShift _ dt) = dt
toDateTime (FallAsleep dt) = dt
toDateTime (WakeUp dt) = dt


expand :: DateTime -> DateTime -> [DateTime]
expand dt1 dt2 =
  (if isSameDayHour dt1 dt2 then dt2 else nextHour dt1)
    & from dt1
    & filter zeroHour


from :: DateTime -> DateTime -> [DateTime]
from dt1 dt2 =
  if dt1 >= dt2 then
    []
  else
    dt1:(from (addMinutes 1 dt1) dt2)


isSameDayHour :: DateTime -> DateTime -> Bool
isSameDayHour dt1 dt2 =
  let
    ( year1, month1, day1, hour1, _, _, _ ) = toParts dt1
    ( year2, month2, day2, hour2, _, _, _ ) = toParts dt2
  in
    year1 == year2 && month1 == month2 && day1 == day2 && hour1 == hour2


nextHour :: DateTime -> DateTime
nextHour dt =
    let
      ( year, month, day, hour, min, sec, ms ) = toParts (addHours 1 dt)
    in
      fromParts ( year, month, day, hour, 0, 0, 0 )


toMin :: DateTime -> Int
toMin dt =
  let
    ( _, _, _, _, min, _, _ ) = toParts dt
  in
    min


zeroHour :: DateTime -> Bool
zeroHour dt =
  let
    ( _, _, _, hour, _, _, _ ) = toParts dt
  in
    hour == 0


mostMinutesSlept :: ( Int, Map.Map Int Int ) -> ( Int, Map.Map Int Int ) -> Ordering
mostMinutesSlept ( _, a ) ( _, b ) =
  compare (addAllMinutes b) (addAllMinutes a)


mostSleptMinute :: ( Int, Int ) -> ( Int, Int ) -> Ordering
mostSleptMinute ( _, a ) ( _, b ) = compare b a


largestSleepingMinute :: ( Int, Map.Map Int Int ) -> ( Int, Map.Map Int Int ) -> Ordering
largestSleepingMinute ( _, a ) ( _, b ) =
  compare (largestMinute b) (largestMinute a)


idByMinute :: ( Int, ( Int, Int ) ) -> Int
idByMinute ( id, ( min, _ ) ) = id * min


addAllMinutes :: Map.Map Int Int -> Int
addAllMinutes =
  foldl (+) 0 . fmap snd . Map.toList


largestMinute :: Map.Map Int Int -> Int
largestMinute m =
  m
    & Map.toList
    & sortBy mostSleptMinute
    & head
    & snd
