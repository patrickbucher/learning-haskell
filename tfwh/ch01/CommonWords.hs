import Data.Char (toLower)
import Data.List (sort, sortBy, words)

-- usage: putStr $ commonWords 3 "to be or not to be"
commonWords :: Int -> String -> String
commonWords n =
  concat . map showRun . take n .  sortRuns . countRuns .
  sortWords .  words . map toLower

sortWords :: [String] -> [String]
sortWords = sort

countRuns :: [String] -> [(Int, String)]
countRuns words = 
  let
    singles = map (\w -> (1, w)) words
  in
    foldl combine [] singles
  where
    combine :: [(Int, String)] -> (Int, String) -> [(Int, String)]
    combine [] (_, w) = [(1, w)]
    combine (h@(lc, lw):t) (_, w) =
      if lw == w
      then ((lc + 1, w):t)
      else ((1, w):(h:t))

sortRuns :: [(Int, String)] -> [(Int, String)]
sortRuns = sortBy (\(lc, _) (rc, _) -> compare rc lc)

showRun :: (Int, String) -> String
showRun (n, w) = w ++ ":\t" ++ show n ++ "\n"
