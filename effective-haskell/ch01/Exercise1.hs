factorial n
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)
