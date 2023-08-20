-- 3.1
a :: [Char]
a = ['a', 'b', 'c']

b :: (Char, Char, Char)
b = ('a', 'b', 'c')

c :: [(Bool, Char)]
c = [(False, '0'), (True, '1')]

d :: ([Bool], [Char])
d = ([False, True], ['0', '1'])

e :: [[t] -> [t]]
e = [tail, init, reverse]

-- 3.2
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- 3.3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 3.4 ...

-- 3.5 The Halting Problem prevents such a comparison. The functions
-- would have to be executed with all the possible inputs, and their
-- results compared. This is only possible for functions operating on
-- types that have a finite set of possible values; e.g. for booleans.
