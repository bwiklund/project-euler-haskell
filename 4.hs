main = print problem4

problem4 = maximum $ filter isPalendromicNumber [x * y | x <- [100..999], y <- [100..999]]

isPalendromicNumber n = str == reverse str where str = show n
