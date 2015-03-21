main = print problem23

problem23 = nonAbundantNumbers

nonAbundantNumbers = filter (not . isAbundant) [1..28123]

factors n = [x | x <- [1..(n `quot` 2)], n `rem` x == 0]

isAbundant n = sum (factors n) > n
