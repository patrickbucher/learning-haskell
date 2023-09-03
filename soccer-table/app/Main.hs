module Main where
import Text.Regex.Base
import Text.Regex.TDFA

parseResult :: String -> (String,Int,Int,String)
parseResult raw = (homeTeam,homeGoals,awayGoals,awayTeam)
  where
    homeTeam = matches !! 1
    homeGoals = read (matches !! 2) :: Int
    awayGoals = read (matches !! 3) :: Int
    awayTeam = matches !! 4
    matches = getAllTextSubmatches (raw =~ "(.+) ([[:digit:]]+):([[:digit:]]+) (.+)")

main :: IO ()
main = putStrLn "Hello, World!"

