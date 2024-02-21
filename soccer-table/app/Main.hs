module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import SoccerTable (parseResult, toTableRows)

main :: IO ()
main = do
  [file] <- getArgs
  text <- TIO.readFile file
  print $ take 10 $ concat $ map toTableRows $ map parseResult $ map T.unpack $ T.split (== '\n') text
