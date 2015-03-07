import System.IO

main = readFile "18.txt" >>= print . problem18 . formatInput

formatInput :: String -> [[Int]]
formatInput = map (map read . words) . lines

-- TODO: use an array not a list
problem18 rows = recMaximumPath 0 0
  where recMaximumPath x y =
          if y < length rows
            then (rows !! y !! x) + (max (recMaximumPath x (y+1)) (recMaximumPath (x+1) (y+1)))
            else 0
