{-
 - Problem 21
 -
 - Insert an element at a given position into a list.
 -
 - Example:
 -
 - * (insert-at 'alfa '(a b c d) 2)
 - (A ALFA B C D)
 - Example in Haskell:
 -
 - λ> insertAt 'X' "abcd" 2
 - "aXbcd"
 -}

import Control.Exception( assert )

insertAt ele list i = 
	(take (i-1) list) ++ [ele] ++ (drop (i-1) list)


insertAt' ele list idx = 
	insertAt'' ele [] list idx
	where
		insertAt'' ele left (r:rxs) idx
			| (idx == 1) = left ++ [ele] ++ (r:rxs)
			| rxs == [] = left ++ [r, ele]
			| otherwise = insertAt'' ele (left ++ [r]) rxs (idx-1)

main = do
	putStr $ assert ( (insertAt 'X' "abcd" 1) == "Xabcd" ) ""
	putStr $ assert ( (insertAt 'X' "abcd" 2) == "aXbcd" ) ""
	putStr $ assert ( (insertAt 'X' "abcd" 3) == "abXcd" ) ""
	putStr $ assert ( (insertAt 'X' "abcd" 4) == "abcXd" ) ""
	putStr $ assert ( (insertAt 'X' "abcd" 5) == "abcdX" ) ""

	putStr $ assert ( (insertAt' 'X' "abcd" 1) == "Xabcd" ) ""
	putStr $ assert ( (insertAt' 'X' "abcd" 2) == "aXbcd" ) ""
	putStr $ assert ( (insertAt' 'X' "abcd" 3) == "abXcd" ) ""
	putStr $ assert ( (insertAt' 'X' "abcd" 4) == "abcXd" ) ""
	putStr $ assert ( (insertAt' 'X' "abcd" 5) == "abcdX" ) ""

