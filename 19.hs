main = print problem19

-- day, and dayOfWeek are zero indexed
-- month has it's own data type
data Date = Date
  { year :: Int
  , month :: Month
  , day :: Int
  , dayOfWeek :: Int
  } deriving (Eq, Show)



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
           deriving (Eq, Show, Ord, Enum)

daysInMonth (Date y m d dow)
  | m == September = 30
  | m == April     = 30
  | m == June      = 30
  | m == November  = 30
  | m == February  = 28 -- TODO leap year
  | otherwise      = 31

problem19 =
  let startDate = Date 1900 January 0 0
  in startDate
