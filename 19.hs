main = print problem19

-- day, and dayOfWeek are zero indexed
-- month has it's own data type
data Date = Date
  { year :: Int
  , month :: Month
  , dayOfMonth :: Int
  , dayOfWeek :: Int
  } deriving (Eq, Show)

-- is this silly? just use ints directly instead?
data Month = January
           | February
           | March
           | April
           | June
           | July
           | August
           | September
           | November
           | December
           deriving (Eq, Show, Ord, Enum, Bounded)

-- this requires the year as well for leap year rules,
-- so we just pass entire dates to it.
daysInMonth :: Date -> Int
daysInMonth (Date y m dom dow)
  | m == September = 30
  | m == April     = 30
  | m == June      = 30
  | m == November  = 30
  | m == February  = 28 -- TODO leap year
  | otherwise      = 31

-- nextDate :: Date -> Date
nextDate date@(Date y m dom dow) =
  let nextDow = (dow + 1) `rem` 7
      (monthOverflow, nextDom) = (dom + 1) `quotRem` (daysInMonth date)
      (yearOverflow, nextMonthN) = ((fromEnum m) + monthOverflow) `quotRem` (fromEnum $ (maxBound :: Month))
      nextMonth = toEnum nextMonthN
      nextYear = y + yearOverflow
  in Date nextYear nextMonth nextDom nextDow

datesFrom = iterate nextDate

problem19 = take 1000 $ datesFrom $ Date 1900 January 0 0
