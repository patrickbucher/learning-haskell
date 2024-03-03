module ExerciseA where

double :: Integer -> Integer
double x = 2 * x

-- map double [1,4,4,3]
--[2,8,8,6]
--
-- map (double . double) [1,4,4,3]
-- [4,16,16,12]

-- map double []
-- []

-- sum . map double = double . sum
-- true: law of distribution 2*(1+2+3)=(2*1+2+2+2*3)

-- sum . map sum = sum . concat
-- true: (1+2+3) + (4+5+6) = 6 + 15 = 21 = (1+2+3+4+5+6)
-- check:
-- sum . map sum $ [[1,2,3],[4,5,6]]
-- 21
-- sum . concat $ [[1,2,3],[4,5,6]] 
-- 21

-- sum . sort = sum
-- true: commutative law 3+1+2 = 1+2+3
-- check:
-- sum [3,1,2]
-- 6
-- import Data.List (sort)
-- sum . sort $ [3,1,2]
-- 6
