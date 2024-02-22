module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import SoccerTable (parseResult, toTableRows)

main :: IO ()
main = do
  [file] <- getArgs
  text <- TIO.readFile file
  let results = concat $ map toTableRows $ map parseResult $ map T.unpack $ T.split (== '\n') text
  print $ take 10 results
  -- TODO: use foldl to group rows by name, which creates a Data.Map
