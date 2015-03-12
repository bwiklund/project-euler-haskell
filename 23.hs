main = print problem23

problem23 = factors 10

factors n = [x | x <- [1..(n `quot` 2)], n `rem` x == 0]
