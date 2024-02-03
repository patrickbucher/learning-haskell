concatMapFoldl f ls = foldl (\acc -> \l -> acc <> f l) [] ls
concatMapFoldr f ls = foldr (\l -> \acc -> f l <> acc) [] ls

-- The implementations are virtually identical, except for the argument order given by foldl/foldr.
