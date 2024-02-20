module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import SoccerTable (parseResult)

main :: IO ()
main = do
  [file] <- getArgs
  text <- TIO.readFile file
  print $ take 10 $ map parseResult $ map T.unpack $ T.split (== '\n') text
