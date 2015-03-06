main = print problem9

problem9 = specialPythagoreanTriplet 1000

specialPythagoreanTriplet n =
  head [a * b * (n-a-b) | a <- [1..n], b <- [1..n], a^2 + b^2 == (n-a-b)^2]
