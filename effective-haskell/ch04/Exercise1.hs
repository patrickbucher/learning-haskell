data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

showStringTree :: BinaryTree String -> String
showStringTree Leaf = ""
showStringTree (Branch l v r) = showStringTree l <> " " <> v <> " " <> showStringTree r

tree :: BinaryTree String
tree = Branch (Branch Leaf "a" Leaf) "b" (Branch Leaf "c" Leaf)

-- showStringTree tree
-- " a b c "
