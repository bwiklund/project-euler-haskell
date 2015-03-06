import System.IO

-- load the file, strip newlines, convert to ints
main = readFile "8.txt" >>= print . problem8 . stringToInts . concat . lines

stringToInts = map (strToInt . (:[]))
  where strToInt = read :: String -> Int

-- the actual problem
problem8 xs = maximum $ productWindows 13 xs

productWindows _ [] = []
productWindows width xs = (product $ take width xs) : (productWindows width $ tail xs)
