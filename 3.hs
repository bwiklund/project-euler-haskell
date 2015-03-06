main = print $ problem3 600851475143


problem3 n = last $ primeFactors n 2

primeFactors :: Int -> Int -> [Int]
primeFactors 1 _ = []
primeFactors n m =
  if n `rem` m == 0 then m : primeFactors (n `div` m) m
                    else primeFactors n (m+1)
