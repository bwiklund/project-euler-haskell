main = print $ problem30 5

problem30 exp = scanl1 (+) $ allSumOfDigitPowers exp

allSumOfDigitPowers exp = filter (isSumOfDigitPowers exp) [10..]

isSumOfDigitPowers exp x =
  let digits = map charDigitToInt (show x)
      charDigitToInt = (read :: String -> Int) . (:[])
      total = sum $ map (^ exp) $ digits
  in total == x
