-- Functors: Class of types that support a mapping function.
inc :: Functor f => f Int -> f Int
inc = fmap (+1)

sqr :: Functor f => f Int -> f Int
sqr = fmap (^2)

-- Functor Laws
-- 1) fmap id = id
-- 2) fmap (g . h) = fmap g . fmap h

-- Applicatives: Functors that allow for the application of multi-arity functions.

-- Applicative Laws
-- 1) pure id <*> x = x
-- 2) pure (g x) = pure g <*> pure x
-- 3) x <*> pure y = pure (\g -> g y) <*> x
-- 4) x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

-- Monads:
data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do
  m <- eval x
  n <- eval y
  safediv m n
