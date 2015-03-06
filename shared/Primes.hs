-- no sieve here because we might not know the size.
-- also i have implimented primes stuff 1000 times already.

module Shared.Primes where

primes = 2 : primes' 3

primes' n = if isPrime n then n : primes' (n+1) else primes' (n+1)

isPrime n = all (/=0) $ map (n `rem`) [2,3..floor $ sqrt $ fromIntegral n]

primeFactors :: Int -> Int -> [Int]
primeFactors 1 _ = []
primeFactors n m =
  if n `rem` m == 0 then m : primeFactors (n `div` m) m
                    else primeFactors n (m+1)
