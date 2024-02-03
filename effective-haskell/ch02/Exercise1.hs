reverseFoldl list = foldl (\acc -> \e -> e : acc) [] list
reverseFoldr list = foldr (\e -> \acc ->  acc <> [e]) [] list

-- The foldl implementation is more straightforward, because of the way lists
-- are created: starting from its head. So the leftmost value in the original
-- lists becomes the rightmost in the reversed list quite naturally. The foldr
-- implementation is not only more involved, it also requires list
-- concatenations, whereas foldl can build up the list from the head.
