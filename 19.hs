main = print problem19

-- month, day, and dayOfWeek are zero indexed
-- dow is monday based
data Date = Date
  { year :: Int
  , month :: Int
  , dayOfMonth :: Int
  , dayOfWeek :: Int
  } deriving (Eq, Show)

-- this requires the year as well for leap year rules,
-- so we just pass entire dates to it.
-- months are ZERO indexed
daysInMonth :: Date -> Int
daysInMonth (Date y m dom dow)
  | m == 8    = 30
  | m == 3    = 30
  | m == 5    = 30
  | m == 10   = 30
  | m == 1    = 28 -- TODO leap year
  | otherwise = 31

-- nextDate :: Date -> Date
nextDate date@(Date y m dom dow) =
  let nextDow = (dow + 1) `rem` 7
      (monthOverflow, nextDom) = (dom + 1) `quotRem` (daysInMonth date)
      (yearOverflow, nextMonth) = (m + monthOverflow) `quotRem` 12
      nextYear = y + yearOverflow
  in Date nextYear nextMonth nextDom nextDow

datesFrom = iterate nextDate

problem19 = takeWhile (\d -> (year d) < 2001) $ datesFrom $ Date 1900 0 0 0
