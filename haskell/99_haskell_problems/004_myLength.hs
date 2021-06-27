{- Find the number of elements of a list. -}

import Control.Exception( assert )

myLength lst =
	myLength_ lst 0
	where
		myLength_ [] i = i
		myLength_ (x:xs) i = myLength_ xs (i+1)

myLength' [] = 0
myLength' (x:xs) = 1 + myLength' xs

myLength'' lst = foldr (\x acc -> acc+1) 0 lst

main = do
	putStr $ assert ( (myLength []) == 0 ) ""
	putStr $ assert ( (myLength [123, 456, 789]) == 3 ) ""
	putStr $ assert ( (myLength "Hello, world!") == 13 ) ""

	putStr $ assert ( (myLength' []) == 0 ) ""
	putStr $ assert ( (myLength' [123, 456, 789]) == 3 ) ""
	putStr $ assert ( (myLength' "Hello, world!") == 13 ) ""

	putStr $ assert ( (myLength'' []) == 0 ) ""
	putStr $ assert ( (myLength'' [123, 456, 789]) == 3 ) ""
	putStr $ assert ( (myLength'' "Hello, world!") == 13 ) ""
