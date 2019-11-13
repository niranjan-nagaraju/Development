{- Number of trailing zeroes in factorial -}

import IO

numZeroes :: Integer -> Integer -> Integer -> Integer
numZeroes a zeroes divisor =
	let tmp = (divisor * 5) in
		if a < tmp
			then zeroes
			else 
				numZeroes a (zeroes+ (a `div` tmp)) tmp
				
getInputAndPrint n = do
	if n == 0
		then return ()
		else do
			a <- (readLn::IO Integer)
			print $ numZeroes a 0 1
			getInputAndPrint (n-1)

main = do
	n <- readLn
	getInputAndPrint n
