main = print $ problem7 10001

problem7 n = last $ take n $ primes

primes = 2 : primes' 3

-- no sieve here because we might not know the size.
-- also i have implimented primes stuff 1000 times already.
primes' n = if isPrime n then n : primes' (n+1) else primes' (n+1)

isPrime n = all (/=0) $ map (n `rem`) [2,3..floor $ sqrt $ fromIntegral n]
