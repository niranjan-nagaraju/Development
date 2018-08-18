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

{- Test execution:
 -
 - *Main> readPair
 - 1 2
 - (1,2)
 -}
readPair :: IO (Int, Int)
readPair = do
	pairStr <- getLine
	let pairs =  ( read $ (words pairStr) !! 0 :: Int,  read $ (words pairStr) !! 1 :: Int )
	return pairs
	

{- Test execution:
 -
 - *Main> replicateM 5 readPair 
 - 1 2
 - 3 4
 - 5 6
 - 7 8
 - 9 1
 - [(1,2),(3,4),(5,6),(7,8),(9,1)]
 -
 - *Main> readTestCase
 - 5
 - 1 2
 - 3 4
 - 5 6
 - 7 8
 - 9 10
 - [(1,2),(3,4),(5,6),(7,8),(9,10)]
 -
 -}
readTestCase :: IO [(Int, Int)]
readTestCase = do
	nLines <- readLn :: IO Int
	replicateM nLines readPair


{- Test execution
 - *Main> processTestCase 
 - 3
 - 1 2
 - 4 5
 - 6 7
 - "YES"
 -}
processTestCase :: IO String
processTestCase = do
	-- Read testcase input
	inputPairs <- readTestCase	
	-- return ("YES", inputPairs) -- return YES and inputpairs for debug
	return "YES" -- Return "YES" for now 

{- Test execution when both string and input list is returned 
 - *Main> main
 - 2
 - 2
 - 1 1
 - 2 2
 - 3
 -  4 1
 -  5 1
 -  6 1
 -  [("YES",[(1,1),(2,2)]),("YES",[(4,1),(5,1),(6,1)])]
 -}  
main = do
	nCases <- readLn :: IO Int
	results <- replicateM nCases processTestCase
	print results


