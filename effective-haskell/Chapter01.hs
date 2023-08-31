factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

curry' :: ((a,a) -> a) -> (a -> a -> a)
curry' f = \x -> \y -> f (x,y)

uncurry' :: (a -> a -> a) -> ((a,a) -> a)
uncurry' f = \(x,y) -> f x y
