{- Find the last element of a list. -}

import Control.Exception (assert)

emptyListError = error "List is empty"

myLast [] = emptyListError
myLast (x:xs) =
	if (null xs)
		then x
		else myLast xs

myLast' [] = emptyListError
myLast' [x] = x
myLast' (x:xs) = myLast' xs

-- foldl1 uses lst[0] as initial accumulator
-- \_ x -> returns second element passed as accumulator as-is
myLast'' = foldl1 (\_ x -> x)

main = do
	putStr $ assert ( (myLast [1,2,3,4]) == 4 ) ""
	putStr $ assert ( (myLast ['x', 'y', 'z']) == 'z' ) ""

	putStr $ assert ( (myLast' [1,2,3,4]) == 4 ) ""
	putStr $ assert ( (myLast' ['x', 'y', 'z']) == 'z' ) ""

	putStr $ assert ( (myLast'' [1,2,3,4]) == 4 ) ""
	putStr $ assert ( (myLast'' ['x', 'y', 'z']) == 'z' ) ""
