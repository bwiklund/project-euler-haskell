import Shared.Primes

main = print problem10

problem10 = sumOfPrimesBelow 2000000

sumOfPrimesBelow n = sum $ takeWhile (< n) primes
