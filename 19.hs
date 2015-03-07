main = print problem19

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

-- day, and dayOfWeek are zero indexed
-- month has it's own data type
data Date = Date
  { year :: Int
  , month :: Month
  , day :: Int
  , dayOfWeek :: Int
  } deriving (Eq, Show)

problem19 =
  let startDate = Date 1900 January 0 0
  in startDate
