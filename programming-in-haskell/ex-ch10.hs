-- 10.1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- 10.2, 10.3
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard rows = sequence_ [putRow i r | (i,r) <- zip [1..] rows]

-- 10.4
adder :: IO ()
adder = do
  putStr "How many numbers? "
  -- TODO
