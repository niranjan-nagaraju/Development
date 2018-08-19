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
import Data.List


{- read a space-separated pair and return a haskell tuple as (a,b)  -}
readPair :: IO (Int, Int)
readPair = do
	pairStr <- getLine
	--let pairs =  ( read $ (words pairStr) !! 0 :: Int,  read $ (words pairStr) !! 1 :: Int )
	let pair' = words pairStr 
	let pairs =  ( (read (head pair') :: Int),  (read (last pair') :: Int) )
	return pairs

{- Test execution:
 -
 - *Main> readPair
 - 1 2
 - (1,2)
 -}


{- Read a single testcase, n followed by n pairs, and return n pairs as a list -}
readTestCase :: IO [(Int, Int)]
readTestCase = do
	nLines <- readLn :: IO Int
	{-
	 -   NOTE: replicateM repeats an IO operation n times, and collects the results in a list
	 -      *Main Data.List> replicateM 5 readPair 
	 -      1 2
	 -      3 4
	 -      5 6
	 -      7 8
	 -      9 10
	 -      [(1,2),(3,4),(5,6),(7,8),(9,10)]
	 -}
	replicateM nLines readPair

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



isFunction :: IO String
isFunction = do
	-- Read testcase input
	inputPairs <- readTestCase

	{-
	-- Get only the function keys,
	-- If there are two same keys that map to different outputs, then it's not a valid function,
	-- However, the mapping from same key to a same output could be repeated in the input
	--  e.g. f(1) = 2, f(2) = 3, f(1) = 2 [This is still a valid function as f(1) is always 2]
	--  Weed such duplicate f(x) = y mapping first, then look to see if the remaining function mappings
	--  do not have two differents for a given x.
	--
	--  NOTE: nub returns unique elements from a list
	--     *Main Data.List> nub [(1,2), (1,2), (4,3)]
	--     [(1,2),(4,3)]
	--     *Main Data.List> nub [1,2,1,3]
	--     [1,2,3]
	-}     
	let uniq_func_pairs = nub inputPairs
	let	uniq_func_keys = fst (unzip uniq_func_pairs)

	if length (nub uniq_func_keys) /= length (uniq_func_keys)
		then return "NO"
		else return "YES"

{- Test execution
 -
 - TC1:
 - *Main Data.List> isFunction 
 - 3
 - 1 2
 - 1 3
 - 2 5
 - "NO"
 -
 - TC2:
 - *Main Data.List> isFunction 
 - 4
 - 1 2
 - 2 3
 - 3 4
 - 4 5
 - "YES"
 -
 - TC3:
 - *Main Data.List> isFunction 
 - 3
 - 1 2
 - 1 2
 - 2 3
 - "YES"
 - 
 - TC4:
 - *Main Data.List> isFunction 
 - 3
 - 1 2
 - 1 3
 - 1 2
 - "NO"
 -
 -}	


{- 
 - read number of input testcases
 - Call isFunction() to read each TC and return "YES" or "NO" for a single TC
 - Use replicateM to repeat isFunction() n times, and collect the results in a list
 -}
main :: IO ()
main = do
	nCases <- readLn :: IO Int
	{- TODO: 
	 - Try substituting this with a more traditional loop
	 - forM [1..n] 
	 -    tc <- readTestCase
	 -    result = isFunction tc
	 -    print list
	 -}    
	results <- replicateM nCases isFunction
	print results

{-
 - Test Program execution:
 - [13:33:19 functions_or_not]$ cat test_input 
 - 5
 - 4
 - 1 2
 - 1 3
 - 2 4
 - 2 4
 - 3
 - 1 2
 - 3 4
 - 5 6
 - 4
 - 1 2
 - 1 3
 - 2 4
 - 3 5
 - 3
 - 1 1
 - 2 2
 - 3 3
 - 1
 - 1 2
 - [13:33:21 functions_or_not]$ cat test_input |  runghc functions_or_not.hs 
 - ["NO","YES","NO","YES","YES"]
 -}
