import           Data.Char

-- 5.1
first_n_squares :: Integral a => a -> a
first_n_squares n = sum [ i ^ 2 | i <- [1 .. n] ]

-- 5.2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (i, j) | i <- [0 .. m], j <- [0 .. n] ]

-- 5.3
square :: Int -> [(Int, Int)]
square n = [ (i, j) | (i, j) <- grid n n, i /= j ]

-- 5.4
my_replicate :: Integral b => b -> a -> [a]
my_replicate n e = [ e | _ <- [1 .. n] ]

-- 5.5
pyths :: Integral a => a -> [(a, a, a)]
pyths n =
  [ (x, y, z)
  | x <- [1 .. n]
  , y <- [1 .. n]
  , z <- [1 .. n]
  , x ^ 2 + y ^ 2 == z ^ 2
  ]

-- 5.6
factors :: Int -> [Int]
factors n = [ i | i <- [1 .. n], n `mod` i == 0 ]

perfects :: Int -> [Int]
perfects n = [ i | i <- [1 .. n], sum [ j | j <- factors i, j < i ] == i ]

-- 5.7
compr = concat [ ([ (x, y) | y <- [3, 4] ]) | x <- [1, 2] ]

-- 5.8
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [ v | (k', v) <- t, k == k' ]

positions :: (Eq a, Integral b) => a -> [a] -> [b]
positions v xs = find v (zip xs [0 ..])

-- 5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct as bs = sum [ a * b | (a, b) <- zip as bs ]

-- 5.10
lowlet2int :: Char -> Int
lowlet2int c = ord c - ord 'a'

int2lowlet :: Int -> Char
int2lowlet n = chr (ord 'a' + n)

caplet2int :: Char -> Int
caplet2int c = ord c - ord 'A'

int2caplet :: Int -> Char
int2caplet n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2lowlet ((lowlet2int c + n) `mod` 26)
          | isUpper c = int2caplet ((caplet2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs ]
