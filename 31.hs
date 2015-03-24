main = print $ problem31 [1, 2, 5, 10, 20, 50, 100, 200] 200

problem31 values target = length $ solutions counts values target
  where counts = map (*0) values -- lazy

-- recursively add values to the left column
solutions [] _ _ = []
solutions counts values target = validSolutions
  where nextCounts = (1 + head counts) : tail counts
        nextTotal = valueOf nextCounts values
        validSolutions = case nextTotal `compare` target of
          GT -> solutions (tail nextCounts) (tail values) (target)
          EQ -> nextCounts : solutions (tail nextCounts) (tail values) (target - (head nextCounts) * (head values))
          LT -> solutions nextCounts values target

valueOf counts values = sum $ zipWith (*) counts values

-- FIXME
