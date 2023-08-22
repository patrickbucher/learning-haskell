import           Data.Char

-- 7.1
f x = x * 2
p x = x `mod` 2 == 0
xs = [0, 1, 2, 3, 4, 5]
ys = map f (filter p xs)

-- 7.2
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = foldr (&&) True (map p xs)

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = foldr (||) False (map p xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x : xs) | p x       = [x] ++ takeWhile' p xs
                      | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x : xs) | p x       = dropWhile p xs
                      | otherwise = xs

-- 7.3
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr ((:) . f) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr ((++) . \x -> if p x then [x] else []) [] xs


-- 7.4
dec2int :: [Int] -> Int
dec2int xs = foldl (\acc -> \x -> acc * 10 + x) 0 xs

-- 7.5
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x -> \y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

-- 7.6
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin' = unfold (== 0) (`mod` 2) (`div` 2)
chop8' = unfold null (take 8) (drop 8)
map'' f = unfold null (f . head) tail
iterate' f = unfold (\_ -> False) id f

-- 7.7
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (parity . make8 . int2bin . ord)

parity :: [Bit] -> [Bit]
parity bits = bits ++ [sum bits `mod` 2]

chopn :: Int -> [Bit] -> [[Bit]]
chopn 0 _    = []
chopn _ []   = []
chopn n bits = take n bits : chopn n (drop n bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . strip_parity) . chopn 9

strip_parity :: [Bit] -> [Bit]
strip_parity bits = if sum payload `mod` 2 == parity_bit
  then payload
  else error "parity check failed"
 where
  payload    = take 8 bits
  parity_bit = last bits

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- 7.8
faulty_channel :: [Bit] -> [Bit]
faulty_channel = tail

faulty_transmit :: String -> String
faulty_transmit = decode . faulty_channel . encode

-- 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []       = []
altMap f g (x : xs) = f x : altMap g f xs

-- 7.10
luhn :: [Int] -> Bool
luhn numbers = sum (altMap id luhnDouble (reverse numbers)) `mod` 10 == 0

luhnDouble :: Int -> Int
luhnDouble x = if y > 9 then y - 9 else y where y = 2 * x
