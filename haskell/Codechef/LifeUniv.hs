{- stop after 42 -}

import IO

getInput = do
	n <- readLn

	if n == 42
		then do
			return () --putStr ""
		else do
			print n
			getInput

main = do
	getInput
