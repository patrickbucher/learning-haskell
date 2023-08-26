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

-- 8.5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g e = case e of
  Val n   -> f n
  Add a b -> g (folde f g a) (folde f g b)

-- 8.6
eval' :: Expr -> Int
eval' e = folde id (+) e

size :: Expr -> Int
size e = folde (\_ -> 1) (+) e

-- 8.7
data Vielleicht a = Nichts | Nur a
instance Eq a => Eq (Vielleicht a) where
  Nichts == Nichts = True
  Nichts == Nur _  = False
  Nur _  == Nichts = False
  Nur a  == Nur b  = a == b

-- 8.8
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop

type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k' ]

eval :: Subst -> Prop -> Bool
eval _ (Const b  ) = b
eval s (Var   x  ) = find x s
eval s (Not   p  ) = not (eval s p)
eval s (And   p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or    p q) = eval s p || eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _  ) = []
vars (Var   x  ) = [x]
vars (Not   p  ) = vars p
vars (And   p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or    p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss where bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs)) where vs = rmdups (vars p)

rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

isTaut :: Prop -> Bool
isTaut p = and [ eval s p | s <- substs p ]

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Not (Var 'A'))

p6 :: Prop
p6 = Equiv (And (Not (Var 'A')) (Not (Var 'B'))) (Not (Or (Var 'A') (Var 'B')))

-- 8.9
data Expression = Value Int
                | Addition Expression Expression
                | Multiplication Expression Expression

value :: Expression -> Int
value e = evaluate e []

type Cont = [Op]

data Op = EVAL_ADD Expression | EVAL_MUL Expression | ADD Int | MUL Int

evaluate :: Expression -> Cont -> Int
evaluate (Value n           ) c = execute c n
evaluate (Addition       x y) c = evaluate x (EVAL_ADD y : c)
evaluate (Multiplication x y) c = evaluate x (EVAL_MUL y : c)

execute :: Cont -> Int -> Int
execute []               n = n
execute (EVAL_ADD y : c) n = evaluate y (ADD n : c)
execute (EVAL_MUL y : c) n = evaluate y (MUL n : c)
execute (ADD      n : c) m = execute c (n + m)
execute (MUL      n : c) m = execute c (n * m)
