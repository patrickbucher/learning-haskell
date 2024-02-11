module Main where
import Test.HUnit
import SoccerTable as ST

testParseResult :: Test
testParseResult =
  let
    raw = "1. FC Haskell 2:3 FC Curry 1918"
    parsed = ST.parseResult raw
    expected = ST.Result "1. FC Haskell" "FC Curry 1918" 2 3
  in
    TestCase (assertEqual "parsed correctly" parsed expected)

tests :: Test
tests = TestList [TestLabel "div" testParseResult]

main :: IO Counts
main = runTestTT tests
