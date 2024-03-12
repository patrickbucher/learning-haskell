import Data.Char (toUpper)

song :: Int -> String
song n =
  if n == 0
  then ""
  else song (n - 1) ++ "\n" ++ verse n

verse :: Int -> String
verse n = line1 n ++ line2 n ++ line3 n ++ line4 n

line1 :: Int -> String
line1 n =
  if n == 1
  then "One man went to mow\n"
  else capitalize (toWord n) ++ " men went to mow\n"

numbers :: [String]
numbers = ["zero", "one", "two", "three", "four", "five",
           "six", "seven", "eight", "nine"]

toWord :: Int -> String
toWord n = numbers !! n

capitalize :: String -> String
capitalize (h:t) = (toUpper h:t)

line2 :: Int -> String
line2 _ = "Went to mow a meadow\n"

line3 :: Int -> String
line3 n =
  if n == 1
  then "One man and his dog\n"
  else capitalize $ concat (map (\w -> w ++ " men, ") (countdown n 2)) ++ "one man and his dog\n"

line4 :: Int -> String
line4 _ = "Went to mow a meadow\n"

countdown :: Int -> Int -> [String]
countdown n m =
  if n < m
  then []
  else (toWord n:countdown (n - 1) m)
