module SoccerTable
  ( Result (Result)
  , parseResult
  ) where
import Text.Regex.Base
import Text.Regex.TDFA

data Result = Result
  { homeTeam :: String
  , awayTeam :: String
  , homeGoals :: Int
  , awayGoals :: Int
  } deriving (Show, Eq)

parseResult :: String -> Result
parseResult raw =
  let
    hT = matches !! 1
    hG = read (matches !! 2) :: Int
    aG = read (matches !! 3) :: Int
    aT = matches !! 4
    matches = getAllTextSubmatches (raw =~ "(.+) ([[:digit:]]+):([[:digit:]]+) (.+)")
  in
    Result hT aT hG aG
