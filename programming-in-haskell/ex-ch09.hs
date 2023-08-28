-- 9.1
subs :: [a] -> [[a]]
subs []       = [[]]
subs (x : xs) = yss ++ map (x :) yss where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms []       = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

-- 9.2
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] []     = True
isChoice [_] []    = False
isChoice [] _      = True
isChoice (x:xs) ys = any (== x) ys && isChoice xs (removeFirst x ys)

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ []     = []
removeFirst x (y:ys) = if x == y then ys else y:(removeFirst x ys)

-- 9.3
-- Pairs with empty lists would just increase the overhead. They
-- are fed into exprs, which justs evaluates them to another empty
-- list, which would not evaluate to anything meaningful.

-- 9.4
