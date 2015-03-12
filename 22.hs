import System.IO
import Data.List.Split
import Data.Char
import Data.List

main = readFile "22.txt" >>= print . problem22 . formatInput
  where formatInput = map (tail . init) . splitOn "," . filter (/= '\n')

problem22 :: [String] -> Int
problem22 strs = sum scores
  where scores = map score (zip (sort strs) [1..])
        

score :: (String, Int) -> Int
score (str, rank) = rank * (sum $ map charToPosition str)

charToPosition :: Char -> Int
charToPosition = (+ (-96)) . ord . toLower
