main = print problem5

problem5 = head [x | x <- [1..], all (==0) $ map (x `rem`) [1..20]]
