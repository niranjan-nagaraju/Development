{- Find the K'th element of a list -}

import Control.Exception( assert )

elementAt (x:xs) i = 
	if i == 1
		then x
		else elementAt xs (i-1)

main = do
	putStr $ assert ( (elementAt [1,2,3] 2) == 2 ) ""
	putStr $ assert ( (elementAt "haskell" 5) == 'e' ) ""
	putStr $ assert ( (elementAt "abcde" 3) == 'c' ) ""
