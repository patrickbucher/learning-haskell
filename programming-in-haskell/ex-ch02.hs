-- 2.1 ...

-- 2.2
a = (2^3)*4

b = (2*3)+(4*5)

c = 2+(3*(4^5))

-- 2.3
n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

-- 2.4
my_last xs = head (reverse xs)

-- 2.5
init1 xs = reverse (tail (reverse xs))
init2 xs = take ((length xs) - 1) xs
