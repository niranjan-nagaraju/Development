{-
 - Filter elements at odd positions in a list
 - [1,2,3,4,5] -> [2,4]
 -}

filter_odd_positions :: [Int] -> [Int]
filter_odd_positions [] = []
filter_odd_positions (x:[]) = []
filter_odd_positions (x:y:xs) = y : (filter_odd_positions xs)

{-
 - REPL
 -
 - *Main> filter_odd_positions [3]
 - []
 - *Main> filter_odd_positions [3,4]
 - [4]
 - *Main> filter_odd_positions [3,4,5]
 - [4]
 - *Main> filter_odd_positions [3,4,5,6]
 - [4,6]
 - *Main> filter_odd_positions [3,4,5,6,7]
 - [4,6]
 -}

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
	inputdata <- getContents
	mapM_ (putStrLn. show). filter_odd_positions. map read. lines $ inputdata


{- 
  Sample runs

 :!printf "1\n2\n3\n4\n" | runghc %  
  2
  4
  Press ENTER or type command to continue

  [15:58:43 functional_programming]$ printf "1\n2\n\3\n4\n5\n6" | runghc filter_odd_positions.hs 
  2
  4
  6
-}



