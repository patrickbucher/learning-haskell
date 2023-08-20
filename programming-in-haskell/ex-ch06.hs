-- 6.1
-- The original implementation would run endlessly.
factorial :: Int -> Int
factorial 0 = 1
factorial n | n < 0 = 0
            | otherwise = n * factorial (n - 1)

-- 6.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 6.3
my_exp :: Int -> Int -> Int
my_exp 0 _ = 0
my_exp b 0 = 1
my_exp b 1 = b
my_exp b e = b * my_exp b (e-1)

-- 6.4
euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a < b = euclid a (b-a)
           | a > b = euclid b (a-b)

-- 6.5
-- length [1,2,3]
-- 1 + length [2,3]
-- 1 + 1 + length [3]
-- 1 + 1 + 1
-- 3

-- drop 3 [1,2,3,4,5]
-- drop 2 [2,3,4,5]
-- drop 1 [3,4,5]
-- drop 0 [4,5]
-- [4,5]

-- init [1,2,3]
-- [1] + init [2,3]
-- [1,2] + init [3]
-- [1,2]

-- 6.6
my_and :: [Bool] -> Bool
my_and [] = True
my_and [x] = x
my_and (x:xs) | x == False = False
              | otherwise = my_and xs

my_concat :: [[a]] -> [a]
my_concat [] = []
my_concat (xs:(y:ys)) = xs ++ y ++ my_concat ys

my_replicate :: Int -> a -> [a]
my_replicate 0 _ = []
my_replicate n e = [e] ++ replicate (n-1) e

my_nth :: [a] -> Int -> a
my_nth (x:_) 0 = x
my_nth (_:xs) n = my_nth xs (n-1)

my_elem :: Eq a => a -> [a] -> Bool
my_elem _ [] = False
my_elem e (x:xs) | e == x = True
                 | otherwise = my_elem e xs

-- 6.7
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = [x] ++ merge xs (y:ys)
                    | otherwise = [y] ++ merge (x:xs) ys

-- 6.8
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = (take i xs, drop i xs)
  where
    i = (length xs) `div` 2
    j = (length xs) - i
    
msort :: Ord a => [a] -> [a]
msort [] = []
msort [e] = [e]
msort xs = merge (msort as) (msort bs)
  where
    (as,bs) = halve xs

-- 6.9
my_sum :: Num a => [a] -> a
my_sum [] = 0
my_sum (x:xs) = x + my_sum xs

my_take :: Int -> [a] -> [a]
my_take _ [] = []
my_take 0 _ = []
my_take n (x:xs) = [x] ++ my_take (n-1) xs

my_last :: [a] -> a
my_last (x:[]) = x
my_last (x:xs) = my_last xs
