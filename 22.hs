import System.IO
import Data.List.Split

main = readFile "22.txt" >>= print . problem22 . formatInput
  where formatInput = map (tail . init) . splitOn "," . filter (/= '\n')

problem22 =
