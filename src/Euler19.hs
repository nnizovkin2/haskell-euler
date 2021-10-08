module Euler19 where
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar

--toWeekDate :: Day -> (Integer, Int, Int)    
--fromGregorian :: Year -> MonthOfYear -> DayOfMonth -> Day
--addDays :: Integer -> Day -> Day
sundaysSum :: Integer
sundaysSum = snd(sundaysR(fromGregorian 1901 1 1, 0))

third :: (a, b, c) -> c
third (_,_,a) = a
sundaysR:: (Day, Integer) -> (Day, Integer)
sundaysR (d, c)
  | d == fromGregorian 2001 1 1 = (d, c)
  | third (toWeekDate d) == 7 && third(toGregorian d) == 1 = sundaysR(addDays 1 d, c+1)
  | otherwise = sundaysR(addDays 1 d, c)