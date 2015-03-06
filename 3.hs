import Shared.Primes

main = print $ problem3 600851475143


problem3 n = last $ primeFactors n 2
