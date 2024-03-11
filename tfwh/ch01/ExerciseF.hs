anagrams :: Int -> [String] -> String
anagrams n words =
  let
    wordsN :: [String]
    wordsN = map fst $ filter (\(_, l) -> l == n) $ map (\w -> (w, length w)) words
    wordsPerms :: [(String, [String])]
    wordsPerms = map (\w -> (w, (filter (\p -> p /= w) (permute w)))) wordsN
    wordsToPerms :: [(String, String)]
    wordsToPerms = map joinPerms wordsPerms
    entries :: [String]
    entries = map joinEntries wordsToPerms
    title :: String
    title = show n <> "-letter words"
    sepLine :: String
    sepLine = concat $ take (length title) (repeat "-")
  in
    joinWith ([title, sepLine] ++ entries) "\n"
  where
    joinPerms :: (String, [String]) -> (String, String)
    joinPerms (word, perms) =
      (word, joinWith perms ",")
    joinEntries :: (String, String) -> String
    joinEntries (word, perms) =
      word <> ": " <> perms

joinWith :: [String] -> String -> String
joinWith elems sep =
  let
    withSep = (("", head elems) : zip (repeat sep) (tail elems))
  in
    concat $ map (\(s, e) -> concat [s, e]) withSep


permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute [x] = [[x]]
permute xs =
  let
    parts = excludeEach xs
  in
    concat $ map combine parts
  where
    combine :: Eq a => (a, [a]) -> [[a]]
    combine (x, xs) = map (\p -> (x:p)) $ permute xs
    excludeEach :: Eq a => [a] -> [(a, [a])]
    excludeEach xs = [(x, filter (/=x) xs) | x <- xs]
