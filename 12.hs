-- TODO: finish this

-- import Shared.Primes
import Data.List

main = print $ problem12 500

-- this would work but it's way too slow for the nubmer of factors we want
-- problem12 n = filter (\factors -> length factors > n) $ map (flip primeFactors 2) triangleNumbers

triangleNumbers = nums 1 0
  where nums i prev = (i+prev) : nums (i+1) (i+prev)

triangleRoot n = (sqrt (8*n + 1) - 1) / 2

isInt n = n == fromInteger (round n)

isTriangleNumber n = isInt $ triangleRoot n

commonFactors :: [[Int]] -> [Int]
commonFactors factors = foldr1 intersect factors

leastCommonMultiple factors = product $ commonFactors factors

problem12 n = "asdf"
-- problem12 n =
--   let divisors = [1..]
--   in fn
