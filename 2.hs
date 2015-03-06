main = print $ problem2 4000000 1 2

fibs a b = a : b : fibs' a b
  where fibs' a b = (a+b) : fibs' b (a+b)

problem2 len a b = sum $ filter even $ takeWhile (< len) (fibs a b)
