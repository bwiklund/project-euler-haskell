main = print $ allRings !! 500

allRings = iterate ring (0, 0, 0)

ring (0, 0, 0) = (1, 1, 1)
ring (ringNum, lastCorner, cornerSum) =
  let armLength = ringNum * 2
      corners = [lastCorner+armLength, lastCorner+armLength*2..]
      nextCornerSum = cornerSum + (sum $ take 4 corners)
      nextLastCorner = corners !! 3
      nextRingNum = ringNum + 1
  in (nextRingNum, nextLastCorner, nextCornerSum)
