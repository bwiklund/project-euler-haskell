import Data.List

main = print $ problem20 100

problem20 = sum . intToDigits . factorial

intToDigits :: Integer -> [Int]
intToDigits = map (read :: String -> Int) . transpose . (:[]) . show

factorial n = product [1..n]
