import Data.Array

main = print $ problem17 [1..1000]

-- rules relevant ways of spelling numbers from 1-1000
-- 1-9 -> unique
-- 20-99 -> TENS + 1-9 rules
-- 100-999 -> X hundred (and XX)
-- no spaces or hyphens are counted

oneToNineteen n = (listArray (0,19) ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]) ! n

tensPrefixes n = (listArray (2,9) ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]) ! n

sayNumber :: Int -> String
sayNumber n
  | n < 100 = oneToNinetyNine n
  | n < 1000 = hundreds n
  | n == 1000 = "onethousand" -- lazy
  | otherwise = "er"

oneToNinetyNine n
  | n < 20 = oneToNineteen n
  | n < 1000 = twentyToNinetyNine n
  | otherwise = "uh"

hundreds n =
  let (hundos, leftovers) = n `quotRem` 100
  in (oneToNineteen hundos) ++ "hundred" ++
    if leftovers /= 0 then "and" ++ oneToNinetyNine leftovers else ""

twentyToNinetyNine n =
  let tens = n `quot` 10
  in tensPrefixes tens ++ oneToNineteen (n - tens * 10)

problem17 xs = length $ concatMap sayNumber xs
