main = print $ problem30 5

problem30 exp = sum $ allSumOfDigitPowers exp

allSumOfDigitPowers exp = filter (isSumOfDigitPowers exp) [10..upperBoundForExp exp]

-- at some point, the number of digits will be large enough that `9^exp * numDigits` won't be large enough create a number of that many digits. So that's a quick simple way to find an upper bound.
-- i could do this with algebra, or just quick and dirty
upperBoundForExp exp = 10 ^ boundExp
  where boundExp = length $ takeWhile isPossible [1..]
        isPossible = \x -> x * 9^exp > 10 ^ (x-1)

isSumOfDigitPowers exp x =
  let digits = map charDigitToInt (show x)
      charDigitToInt = (read :: String -> Int) . (:[])
      total = sum $ map (^ exp) $ digits
  in total == x
