{- Find out whether a list is a palindrome. -}

import Control.Exception( assert )

isPalindrome lst = (reverse lst) == lst

{- Use two-pointers to check for palindromes -}
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' (x:xs) = ((last xs) == x) && isPalindrome' (init xs)

main = do
	putStr $ assert( (not $ isPalindrome [1,2,3]) ) ""
	putStr $ assert( (isPalindrome [3,2,3]) ) ""
	putStr $ assert( isPalindrome "madamimadam" ) ""
	putStr $ assert( isPalindrome [1,2,4,8,16,8,4,2,1] ) ""

	putStr $ assert( (not $ isPalindrome' [1,2,3]) ) ""
	putStr $ assert( (isPalindrome' [3,2,3]) ) ""
	putStr $ assert( isPalindrome' "madamimadam" ) ""
	putStr $ assert( isPalindrome' [1,2,4,8,16,8,4,2,1] ) ""
