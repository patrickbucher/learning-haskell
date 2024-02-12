module SoccerTable
  ( Result (Result)
  , parseResult
  ) where
import Text.Regex.Base
import Text.Regex.TDFA

-- |
-- Result is a parsed soccer game end result.
data Result = Result
  { homeTeam :: String
  -- ^ The name of the home team.
  , awayTeam :: String
  -- ^ The name of the away team.
  , homeGoals :: Int
  -- ^ The amount of goals scored by the home team.
  , awayGoals :: Int
  -- ^ The amount of goals scored by the away team.
  } deriving (Show, Eq)

-- |
-- Parses a string such as \"Foo 3:2 Bar\" to a result with the respective
-- home/away team name and goals.
--
-- >>> parseResult "Foo 3:2 Bar"
-- Result {homeTeam = "Foo", awayTeam = "Bar", homeGoals = 3, awayGoals = 2}
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
