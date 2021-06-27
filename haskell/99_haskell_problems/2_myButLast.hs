{- Find the last but one element of a list. -}

import Control.Exception( assert )

myButLast (x:y:xs) = 
	if (xs == [])
		then x
		else myButLast xs

myButLast' [x,y] = x
myButLast' (x:xs) = myButLast' xs

main = do
	putStr $ assert ( (myButLast [1,2,3,4]) == 3 ) ""
	putStrLn $ assert ( (myButLast ['a' .. 'z']) == 'y' ) ""

	putStr $ assert ( (myButLast' [1,2,3,4]) == 3 ) ""
	putStrLn $ assert ( (myButLast' ['a' .. 'z']) == 'y' ) ""
