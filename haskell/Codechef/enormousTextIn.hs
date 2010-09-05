{- Print how many numbers out of 'n' are divisible by 'k' -}

import IO

{- get n Integers -}
findNumDivs n k ndivs = 
	if n == 0
		then do	print ndivs
		else do
			num <- (readLn::IO Integer)
			if num `mod` k == 0
				then findNumDivs (n-1) k (ndivs + 1)
				else findNumDivs (n-1) k ndivs


main = do
	inStr <- (getLine::IO String)
	let [n, k] = (map read . words $ inStr)::[Integer]
	findNumDivs n k 0


