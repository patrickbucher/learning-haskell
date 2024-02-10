module SoccerTable where
import Text.Regex.Base
import Text.Regex.TDFA

data Result = Result
  { homeTeam :: String
  , awayTeam :: String
  , homeGoals :: Int
  , awayGoals :: Int
  } deriving Show

parseResult :: String -> Result
parseResult raw =
  let
    homeTeam = matches !! 1
    homeGoals = read (matches !! 2) :: Int
    awayGoals = read (matches !! 3) :: Int
    awayTeam = matches !! 4
    matches = getAllTextSubmatches (raw =~ "(.+) ([[:digit:]]+):([[:digit:]]+) (.+)")
  in
    Result homeTeam awayTeam homeGoals awayGoals
