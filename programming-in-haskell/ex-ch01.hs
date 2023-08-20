-- 1.1
x = 2 * succ 1

-- 1.2 ...

-- 1.3
my_product [x     ] = x
my_product (x : xs) = x * my_product xs

--  1.4
qsort [] = []
qsort (x : xs) =
  qsort ([ a | a <- xs, a > x ]) ++ [x] ++ qsort ([ a | a <- xs, a <= x ])

-- 1.5 The sort would switch positions of equal elements.
