{-
 - https://www.hackerrank.com/challenges/30-arrays
 -}

-- Get space separated integers in a line, make an array out of it
-- "1 2 3" -> [1,2,3]
getArrayFromLine :: IO [Int]
getArrayFromLine = do
	arrayStr <- getLine
	return (map read.words $ arrayStr)

-- Array of Int to space-separated string of Ints
-- [1,2,3] -> "1 2 3"
arrayToString array = 
	unwords . map show $ array
	

main = do
	n <- readLn :: IO Int
	array <- getArrayFromLine
	putStrLn (arrayToString $ reverse array)
