import Data.List (sort, nub)

main = print $ problem29 100 100

problem29 an bn = length . nub . sort $ combinations
  where combinations = [a ^ b | a <- [2..an], b <- [2..bn]]
