import Shared.Fibonacci

main = print $ problem25 1000

problem25 n = (length . takeWhile (< (10 ^ (n-1))) $ fibs 1 1) + 1
