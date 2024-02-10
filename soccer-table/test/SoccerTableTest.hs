module Main where
import Test.HUnit
import SoccerTable

testParseResult =
  let
    raw = "1. FC Haskell 2:3 FC Curry 1918"
    parsed = SoccerTable.parseResult raw
  in
    assertEqual "parsed correctly" parsed SoccerTable.Result "1. FC Haskell" "FC Curry 1918" 2 3

main :: IO ()
main = runTestTTAndExit (TestList [TestLabel "testParseResult" testParseResult])
