module SoccerTable
  ( Result (Result)
  , parseResult
  , toTableRows
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
-- TableRow is a row in a League table, representing a team.
data TableRow = TableRow
  { name :: String
  -- ^ The name of the team.
  , gamesWon :: Int
  -- ^ The number of games won.
  , gamesLost :: Int
  -- ^ The number of games lost.
  , gamesTied :: Int
  -- ^ The number of games tied.
  , goalsScored :: Int
  -- ^ The number of goals scored.
  , goalsConceded :: Int
  -- ^ The number of goals conceded.
  , goalsDiff :: Int
  -- ^ The difference of goalsScored and goalsConceded.
  } deriving (Show, Eq)

-- |
-- parseResult parses a string such as \"Foo 3:2 Bar\" to a result with the
-- respective home/away team name and goals.
--
-- >>> parseResult "Foo 3:2 Bar"
-- Result {homeTeam = "Foo", awayTeam = "Bar", homeGoals = 3, awayGoals = 2}
parseResult :: String -> Result
parseResult raw =
  let
    [_, hT, hG, aG, aT] = matches
    matches = getAllTextSubmatches (raw =~ "(.+) ([[:digit:]]+):([[:digit:]]+) (.+)")
  in
    Result hT aT (read hG) (read aG)

-- |
-- toTableRows computes a TableRow (representing the outcome of a single game)
-- for each team.
--- >>> toTableRows Result {homeTeam = "Foo", awayTeam = "Bar", homeGoals + 3, awayGoals = 2}
-- [TableRow {name = "Foo", ...}
--  TableRow {name = "Bar", ...}]
toTableRows :: Result -> [TableRow]
toTableRows r =
  let
    home = TableRow (homeTeam r) 0 0 0 (homeGoals r) (awayGoals r) (homeGoals r - awayGoals r)
    away = TableRow (awayTeam r) 0 0 0 (awayGoals r) (homeGoals r) (awayGoals r - homeGoals r)
  in
    case compare (homeGoals r) (awayGoals r) of
      GT ->
        [ home {gamesWon = 1, gamesLost = 0, gamesTied = 0}
        , away {gamesWon = 0, gamesLost = 1, gamesTied = 0} ]
      LT ->
        [ home {gamesWon = 0, gamesLost = 1, gamesTied = 0}
        , away {gamesWon = 1, gamesLost = 0, gamesTied = 0} ]
      EQ ->
        [ home {gamesWon = 0, gamesLost = 0, gamesTied = 1}
        , away {gamesWon = 0, gamesLost = 0, gamesTied = 1} ]
