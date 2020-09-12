

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate


toDayString i = ["Jan","Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"] !! (i-1)


solve :: Integer -> Integer -> (String, String, Int)
solve a b = (fm, lm, cnt)
    where
      allFirstFriday = [ (y,m) |
                          y <- [a..b], m <- [1..12], 
                          let (year,_,dow) = toWeekDate $ fromGregorian y m 1, 
                          dow==5 && (gregorianMonthLength y m)==31]
      fm = toDayString $ snd $ head allFirstFriday                     
      lm = toDayString $ snd $ last allFirstFriday
      cnt = length allFirstFriday



testDate = toWeekDate $ fromGregorian 2016 1 1

test = gregorianMonthLength 2020 11


