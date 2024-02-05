-- Data.Tuple.swap :: (a,b) -> (b,a)
mySwap :: (a, b) -> (b, a)
mySwap (x, y) = (y, x)
-- > mySwap ("foo", 3)
-- (3,"foo")

-- concat :: [[a]] -> [a]
myConcat :: [[a]] -> [a]
myConcat xss = foldl (<>) [] xss
-- > myConcat [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]

-- id :: a -> a
myId :: a -> a
myId x = x
-- > myId 4
-- 4
