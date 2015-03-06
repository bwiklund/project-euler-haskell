import System.IO
import Data.Array

-- load the file, strip newlines, convert to ints
main = readFile "11.txt" >>= print . problem11 4 . formatFile

formatFile = map (map strToInt . words) . lines
  where strToInt = read :: String -> Int

problem11 lineLength rows =
  let height = length rows
      width = length $ head rows
      rowArrays = map (\row -> listArray (1, length row) row) rows
      colArray = listArray (1, length rowArrays) rowArrays
      allLines = concatMap indexesForDirection [(1,0), (0,1), (1,1), (1,-1)]
      startingIndices = [(x,y) | x <- [1..width], y <- [1..height]]
      indexesForDirection dir = map (makeLine dir) startingIndices
      makeLine (x,y) (sx,sy) = zipWith (,) (take lineLength $ cycle [sx, sx+x..]) (take lineLength $ cycle [sy, sy+y..])
      numAtIndex (x,y) =
        if x >= 1 && x <= width && y >= 1 && y <= height
          then colArray ! x ! y-- TODO: 2d array instead of manually doing the same thing
          else 0 -- we're sloppy about making these lines, just toss values that might go outside.
                 -- TODO: generate lines that wont go out of bounds in the first place
      lineProduct line = product $ map numAtIndex line
  in maximum $ map lineProduct allLines
