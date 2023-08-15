-- 4.1
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
           where n = (length xs) `div` 2
-- 4.2
third :: [a] -> a
third xs = head (tail (tail xs))
-- third xs = xs !! 2
-- third (_:(_:x:_)) = x

-- 4.3
safetail :: [a] -> [a]

-- a
safetail xs = if null xs then xs else tail xs

-- b
-- safetail xs | null xs = xs

-- c
-- safetail xs | otherwise = tail xs

--safetail [] = []
--safetail (x:xs) = xs

-- 4.4
oder :: Bool -> Bool -> Bool
True `oder` True = True
True `oder` False = True
False `oder` True = True
False `oder` False = False

-- True `oder` _ = True
-- _ `oder` True = True
-- _ `oder` _ = False

-- False `oder` False = False
-- _ `oder` _ = True

-- False `oder` x = x
-- _ `oder` _ = True

-- 4.5
und :: Bool -> Bool -> Bool
und x y = if x then if y then True else False else False

-- 4.6
und2 :: Bool -> Bool -> Bool
und2 x y = if x then y else False

-- 4.7
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

-- 4.8
luhnDouble :: Int -> Int
luhnDouble x = if y > 9 then y - 9 else y
               where
                   y = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = x `mod` 10 == 0
               where
                   x = (luhnDouble a) + b + (luhnDouble c) + d
