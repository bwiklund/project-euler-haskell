import Data.List (permutations, sort)

main = print $ concatMap show $ problem24 [0..9] 1000000

problem24 xs n = (lexicalPermutations xs) !! (n - 1)

lexicalPermutations = sort . permutations
