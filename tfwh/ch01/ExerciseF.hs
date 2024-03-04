anagrams :: Int -> [String] -> String
anagrams n words =
  concat $ map fst . filter (\(_, l) -> l == n) $ map (\w -> (w, length w)) words

permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute [x] = [[x]]
permute xs =
  -- TODO: refactor (could be a single expression)
  let
    parts = excludeEach xs
  in
    concat $ map combine parts
  where
    combine :: Eq a => (a, [a]) -> [[a]]
    combine (x, xs) = map (\p -> (x:p)) $ permute xs
    excludeEach :: Eq a => [a] -> [(a, [a])]
    excludeEach xs = [(x, filter (/=x) xs) | x <- xs]
