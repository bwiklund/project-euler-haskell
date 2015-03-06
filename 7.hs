import Shared.Primes

main = print $ problem7 10001

problem7 n = last $ take n $ primes
