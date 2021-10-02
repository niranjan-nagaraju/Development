{-
 - Problem 20
 - (*) Remove the K'th element from a list.
 -
 - Example in Prolog:
 -
 - ?- remove_at(X,[a,b,c,d],2,R).
 - X = b
 - R = [a,c,d]
 - Example in Lisp:
 -
 - * (remove-at '(a b c d) 2)
 - (A C D)
 - (Note that this only returns the residue list, while the Prolog version also returns the deleted element.)
 -
 - Example in Haskell:
 -
 - λ> removeAt 2 "abcd"
 - ('b',"acd")
 -}

import Control.Exception( assert )

{- using take, drop -}
removeAt k list  = 
	(last firstK, (init firstK) ++ drop k list)
	where
		firstK = take k list


{-
 - separate list into left, right,
 - when [left] has k elements,
 -  skip last element from [left], and concat with the [right]
 -  return (the skipped last element, rest of the list)
 -}
removeAt' k lst = removeAt'' k [] lst
	where
	removeAt'' k left right
		| (length left == k) = (last left, init left ++ right)
		| otherwise = removeAt'' k (left ++ [head right]) (tail right) 
	
main = do
	putStr $ assert ( (removeAt 1 "abcd") == ('a', "bcd") ) ""
	putStr $ assert ( (removeAt 2 "abcd") == ('b', "acd") ) ""
	putStr $ assert ( (removeAt 3 "abcd") == ('c', "abd") ) ""
	putStr $ assert ( (removeAt 4 "abcd") == ('d', "abc") ) ""

	putStr $ assert ( (removeAt' 1 "abcd") == ('a', "bcd") ) ""
	putStr $ assert ( (removeAt' 2 "abcd") == ('b', "acd") ) ""
	putStr $ assert ( (removeAt' 3 "abcd") == ('c', "abd") ) ""
	putStr $ assert ( (removeAt' 4 "abcd") == ('d', "abc") ) ""
