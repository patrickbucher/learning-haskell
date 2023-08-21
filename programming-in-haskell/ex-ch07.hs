-- 7.1
f x = x * 2
p x = x `mod` 2 == 0
xs = [0, 1, 2, 3, 4, 5]
ys = map f (filter p xs)

-- 7.2
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = foldr (&&) True (map p xs)

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = foldr (||) False (map p xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x : xs) | p x       = [x] ++ takeWhile' p xs
                      | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x : xs) | p x       = dropWhile p xs
                      | otherwise = xs

-- 7.3
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr ((:) . f) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr ((++) . \x -> if p x then [x] else []) [] xs


-- 7.4
dec2int :: [Int] -> Int
dec2int xs = foldl (\acc -> \x -> acc * 10 + x) 0 xs

-- 7.5
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x -> \y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

-- 7.6
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin = unfold (== 0) (`mod` 2) (`div` 2)
chop8 = unfold null (take 8) (drop 8)
map'' f = unfold null (f . head) tail
iterate' f = unfold (\_ -> False) id f
