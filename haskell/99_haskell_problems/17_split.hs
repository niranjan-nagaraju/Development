{-
 - Problem 17
 - (*) Split a list into two parts; the length of the first part is given.
 -
 - Do not use any predefined predicates.
 -
 - Example:
 -
 - * (split '(a b c d e f g h i k) 3)
 - ( (A B C) (D E F G H I K))
 - Example in Haskell:
 -
 - λ> split "abcdefghik" 3
 - ("abc", "defghik")
 -}

import Control.Exception( assert )

split :: [a] -> Integer -> ([a], [a])
split list n =
	split' list n []
	where
	split' (x:xs) n left
		| ( n > 0 ) = split' xs (n-1) (left ++ [x])	-- Keep collecting to the left until it has 'n' elements
		| otherwise = (left, x:xs)	-- We've collected enough to the left, Add the remainder to the right and return


-- using take and drop
split' list n  = (take n list, drop n list)

main = do
	putStr $ assert ( (split [1,2,3,4,5,6] 4) == ([1,2,3,4], [5,6]) ) ""
	putStr $ assert ( (split "abcdefghik" 3) == ("abc", "defghik") ) ""

	putStr $ assert ( (split' [1,2,3,4,5,6] 4) == ([1,2,3,4], [5,6]) ) ""
	putStr $ assert ( (split' "abcdefghik" 3) == ("abc", "defghik") ) ""
