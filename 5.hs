import Data.List
import Data.Function

main = print $ problem5 20

-- old naive version. can still be used to check answer.
-- problem5naive n = head [x | x <- [1..], all (==0) $ map (x `rem`) [1..n]]

-- copied from 3
primeFactors :: Int -> Int -> [Int]
primeFactors 1 _ = []
primeFactors n m =
  if n `rem` m == 0 then m : primeFactors (n `div` m) m
                    else primeFactors n (m+1)

frequency xs = map (\xs -> (head xs, length xs)) . group . sort $ xs

-- without using mutable state...
-- map the range of factors to their own factors.
-- then, make a list of the bare minimum factors needed.
-- then take the product of all of them.

-- TODO: could this be dried up?
problem5 n =
  let candidates = [1..n]
      factorFactors = map (flip primeFactors $ 2) candidates
      factorUsages = concatMap frequency factorFactors
      countsSorted = sortBy (compare `on` fst) factorUsages
      countsGrouped = groupBy (\x y -> fst x == fst y) countsSorted
      finalCounts = map (head . sortBy ((flip compare) `on` snd)) countsGrouped
      expanded = concatMap (\(factor, power) -> replicate power factor) finalCounts
      finalProduct = foldr1 (*) expanded
  in finalProduct
