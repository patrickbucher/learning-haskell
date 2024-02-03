myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZipWithComprehension f xs ys = [f x y | (x, y) <- zip xs ys]

myZipWithFoldl :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWithFoldl f xs ys = foldl (\acc -> \(x, y) -> f x y : acc) [] (zip xs ys)
