sumUpTo3 = \e t -> if e >= 3 then e else e + t
twice = \x -> x * 2

-- Original:
-- \f g -> foldr g 0 . map f
-- \f g -> foldr (g . f) 0

-- Applications:
a = foldr sumUpTo3 0 . map twice

b :: [Integer] -> Integer
b = foldr (sumUpTo3 . twice) 0

c = foldr (+) 0 . map twice

d :: [Integer] -> Integer
d = foldr ((+). twice) 0


