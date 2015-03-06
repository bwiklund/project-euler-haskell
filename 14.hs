import Data.List

main = print $ problem14 1000000

collatz n
  | even n = n `quot` 2
  | odd n = 3 * n + 1

collatzSequence 1 = []
collatzSequence n =
  let next = collatz n
  in next : collatzSequence next

maxIndex xs = snd $ last $ sort $ zip xs [1..]

problem14 lim = maxIndex $ map (length . collatzSequence) [1..lim]
