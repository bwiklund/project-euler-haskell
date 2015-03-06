main = print $ problem1 [3, 5] 1000

problem1 xs len =
  let mults = [x | x <- [1..len-1], any (==0) $ map (x `mod`) xs]
  in sum mults
