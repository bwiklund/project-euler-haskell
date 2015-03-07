main = print . sum . map ((read :: String -> Int) . (:[])) $ show (2^1000)
