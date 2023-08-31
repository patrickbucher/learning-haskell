-- 10.1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- 10.2
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [] = return ()
putBoard rs = putBoardRows rs $ 1

putBoardRows :: Board -> Int -> IO ()
putBoardRows [] _ = return ()
putBoardRows (r:rs) i = do
  putRow i r
  putBoardRows rs (i+1)
  
-- 10.3
putBoard' :: Board -> IO ()
putBoard' rows = sequence_ [putRow i r | (i,r) <- zip [1..] rows]

-- 10.4
adder :: IO ()
adder = do
  putStr "How many numbers? "
  n <- readInt
  sum <- readIntsSum n 0
  putStr $ "The total is " ++ (show sum) ++ "\n"

readIntsSum :: Int -> Int -> IO Int
readIntsSum 0 acc = return acc
readIntsSum n acc = do
  i <- readInt
  readIntsSum (n-1) (acc+i)

readInt :: IO Int
readInt = do
  line <- readLine
  return (read line :: Int)
  
readLine :: IO String
readLine = do
  c <- getChar
  case c of
    '\n' -> return []
    _ -> do
      cs <- readLine
      return (c:cs)
