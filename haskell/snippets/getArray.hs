-- Get n integers
getArray :: Int -> IO [Int]
getArray 0 = return []
getArray n = do
	x <- readLn :: IO Int
	xs <- getArray (n-1)
	return (x : xs)

{-
 - *Main> getArray 3
 - 5
 - 8
 - 9
 - [5,8,9]
 -} 
