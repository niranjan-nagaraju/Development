{- Reverse a list. -}

import Control.Exception( assert )

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


{- two pointers implementation -}
myReverse' [] = []
myReverse' [x] = [x]
myReverse' (x:xs) = (last xs) : (myReverse' $ init xs) ++ [x]


main = do
	putStr $ assert ( (myReverse "A man, a plan, a canal, panama!") == "!amanap ,lanac a ,nalp a ,nam A" ) ""
	putStr $ assert ( (myReverse [1,2,3,4]) == [4,3,2,1] ) ""
	putStr $ assert ( (myReverse [1,2,3,4,5]) == [5,4,3,2,1] ) ""

	putStr $ assert ( (myReverse' "A man, a plan, a canal, panama!") == "!amanap ,lanac a ,nalp a ,nam A" ) ""
	putStr $ assert ( (myReverse' [1,2,3,4]) == [4,3,2,1] ) ""
	putStr $ assert ( (myReverse' [1,2,3,4,5]) == [5,4,3,2,1] ) ""
