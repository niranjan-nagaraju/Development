-- Get n integers, one on each line and make an array out of it
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

-- Get space separated integers in a line, make an array out of it
getArrayFromLine :: IO [Int]
getArrayFromLine = do
	arrayStr <- getLine
	return (map read.words $ arrayStr)

{-
 - *Main> getArrayFromLine
 - 1 2 3
 - [1,2,3]
 -} 
