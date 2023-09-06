-- Functors: Class of types that support a mapping function.
inc :: Functor f => f Int -> f Int
inc = fmap (+1)

sqr :: Functor f => f Int -> f Int
sqr = fmap (^2)

-- Functor Laws
-- 1) fmap id = id
-- 2) fmap (g . h) = fmap g . fmap h

-- Applicatives: Functors that allow for the application of multi-arity functions.
