anagrams :: Int -> [String] -> String
anagrams n words =
  let
    wordsN = map fst $ filter (\(_, l) -> l == n) $ map (\w -> (w, length w)) words
    wordsPerms = map (\w -> (w, (filter (\p -> p /= w) (permute w)))) wordsN
    wordsToPerms = map (\(w, ps) -> (w, joinWith ps ",")) wordsPerms
    entries = map (\(w, ps) -> w <> ": " <> ps) wordsToPerms
    title = show n <> "-letter words"
    sepLine = concat $ take (length title) (repeat "-")
  in
    joinWith ([title, sepLine] ++ entries) "\n"

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
