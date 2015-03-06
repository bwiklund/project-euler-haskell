import System.IO

-- load stuff
main = readFile "13.txt" >>= print . problem13 . formatFile
formatFile = map (read :: String -> Integer) . lines

-- process it
problem13 = take 10 . show . sum
