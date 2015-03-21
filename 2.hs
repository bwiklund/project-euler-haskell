import Shared.Fibonacci

main = print $ problem2 4000000 1 2

problem2 len a b = sum $ filter even $ takeWhile (< len) (fibs a b)
