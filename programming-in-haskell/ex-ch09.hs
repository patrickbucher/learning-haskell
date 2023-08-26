-- 8.1
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero     n = Zero
mult (Succ m) n = add n (mult m n)

-- 8.2
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y    ) = x == y
occurs x (Node l y r) = case compare x y of
  LT -> occurs x l
  EQ -> True
  GT -> occurs x r
-- Unlike the original definition, only one side of the tree is
-- processed if x and y are not equal.

-- 8.3
data BinaryTree a = BinaryLeaf a | BinaryNode (BinaryTree a) (BinaryTree a)

leafs :: BinaryTree a -> Int
leafs bt = case bt of
  BinaryLeaf _   -> 1
  BinaryNode l r -> leafs l + leafs r

balanced :: BinaryTree a -> Bool
balanced bt = case bt of
  BinaryLeaf _   -> True
  BinaryNode l r -> abs (nl - nr) <= 1   where
    nl = leafs l
    nr = leafs r

-- 8.4
instance (Show a) => Show (BinaryTree a) where
  show (BinaryLeaf x  ) = show x
  show (BinaryNode l r) = "[" ++ show l ++ ":" ++ show r ++ "]"

halve :: [a] -> ([a], [a])
halve []  = ([], [])
halve [x] = ([x], [])
halve xs  = (take n xs, drop n xs) where
  l = length xs
  m = l `div` 2
  n = l - m

balance :: [a] -> BinaryTree a
balance [x] = BinaryLeaf x
balance xs  = BinaryNode (balance l) (balance r) where (l, r) = halve xs
