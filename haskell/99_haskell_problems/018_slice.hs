{-
 - Problem 18
 - (**) Extract a slice from a list.
 -
 - Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
 -
 - Example:
 - * (slice '(a b c d e f g h i k) 3 7)
 - (C D E F G)
 -
 - Example in Haskell:
 - λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
 - "cdefg"
 -}

import Control.Exception( assert )

-- take(drop())
slice :: [a] -> Int -> Int -> [a]
slice list i k =
	take (k-i+1) $ drop (i-1) list

-- drop(take())
slice' :: [a] -> Int -> Int -> [a]
slice' list i k =
	drop (i-1) $ take k list


-- using guards, accumulate by skipping until position i, then counting down till k hits 0
slice'' :: [a] -> Int -> Int -> [a]
slice'' [] _ _ = []
slice'' (x:xs) i k
	| k == 0 = [] -- stop consuming when k hits 0
	| i == 1 = x : slice'' xs 1 (k-1)  -- start consuming i hits 1
	| otherwise = slice'' xs (i-1) (k-1) -- skip first (i-1) items, skipping 1 item => entire list is shifted by 1, so k => k-1

{-	
 -	list
 -	 1  2  3  4  5  6  7  8  9  10
 -  [a, b, c, d, e, f, g, h, i, k]
 -  3..7 => "cdefg"
 -  
 -  skip "a"
 -  list':
 -	 1  2  3  4  5  6  7  8  9
 -  [b, c, d, e, f, g, h, i, k]
 -  3..7 in list => 2..6 in list'
 -}
	
-- alternate implementation of slice'' without the guards
slice''' :: [a] -> Int -> Int -> [a]
slice''' [] _ _ = []
slice''' _ _ 0 = [] -- stop consuming when k hits 0
slice''' (x:xs) 1 k = x : slice''' xs 1 (k-1) -- start consuming when i hits 1
slice''' (_:xs) i k = slice''' xs (i-1) (k-1) -- skip first (i-1) items, skipping 1 item => entire list is shifted by 1, so k => k-1

main = do
	putStr $ assert ( (slice ['a','b','c','d','e','f','g','h','i','k'] 3 7) == "cdefg" ) ""
	putStr $ assert ( (slice [1..10] 2 5) == [2..5] ) ""  

	putStr $ assert ( (slice' ['a','b','c','d','e','f','g','h','i','k'] 3 7) == "cdefg" ) ""
	putStr $ assert ( (slice' [1..10] 2 5) == [2..5] ) ""  

	putStr $ assert ( (slice'' ['a','b','c','d','e','f','g','h','i','k'] 3 7) == "cdefg" ) ""
	putStr $ assert ( (slice'' [1..10] 2 5) == [2..5] ) ""  

	putStr $ assert ( (slice''' ['a','b','c','d','e','f','g','h','i','k'] 3 7) == "cdefg" ) ""
	putStr $ assert ( (slice''' [1..10] 2 5) == [2..5] ) ""  
