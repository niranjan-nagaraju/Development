{- compute factorials of n<= 100 -}

import IO

factorial 0 = 1
factorial n = product [1..n]

getInputAndPrintFact n = 
	if n == 0
		then return ()
		else do
			num <- (readLn::IO Integer)
			print $ factorial num
			getInputAndPrintFact (n-1)

main = do
	n <- (readLn:: IO Integer)
	getInputAndPrintFact n
