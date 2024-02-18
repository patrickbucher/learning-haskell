module Main where

import Control.Monad
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import System.IO (openFile, hGetContents, IOMode(ReadMode))

import SoccerTable (parseResult)

main :: IO ()
main = do
  args <- getArgs
  lines <- readLines (args !! 0)
  print $ map parseResult lines

readLines :: String -> IO [String]
readLines path = do
  handle <- openFile path ReadMode
  content <- hGetContents handle
  let lines = splitOn "\n" content
  return $ filter (\l -> l /= "") lines
