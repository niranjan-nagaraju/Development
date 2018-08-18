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

import Control.Monad

readPair :: IO (Int, Int)
readPair = do
	pairStr <- getLine
	let pairs =  ( (read.words $ pairStr !! 0) :: Int,  (read.words $ pairStr !! 1) :: Int )
	return pairs
	

readTestCases :: IO Int -> IO [(Int, Int)]
readTestCases nCases = do
	nLines <- readLn :: IO Int
	let cases = forM [1..nLines] readPair
	return cases

main = do
	nCases <- readLn :: IO Int
	print $ readTestCases nCases

