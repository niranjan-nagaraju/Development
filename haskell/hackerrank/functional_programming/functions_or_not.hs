{-
 - https://www.hackerrank.com/challenges/functions-or-not/problem
 -
 - (x,y) pair is a function if f(x) is always y, if there exists a pair (x,y) and (x,z),
 - then the mapping is NOT a function
 -
 - Sample Input
 -
 - 2  
 - 3  
 - 1 1  
 - 2 2  
 - 3 3  
 - 4
 - 1 2
 - 2 4
 - 3 6  
 - 4 8  
 -
 - Sample Output
 -
 - YES  
 - YES
 -
 -}

readSingleTestCase = do
	

readTestCases :: IO Int -> IO [Int, Int]
readTestCases nCases = do
	let n <- readLn :: IO Int
	let cases <- readSingleTestCase
	return cases

main = do
	let	nCases <- readLn :: IO Int
	readTestCases nCases
    print $ numbers

