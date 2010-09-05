import IO

main = do
	{- n s.t. 
	 - (n * log phi) - (log 5) / 2 = (1000 * log 10)
	 -}
	let n = ((1000 * log 10) + ((log 5) / 2)) / (log ((1+ sqrt 5)/2))
	{- 4786.644242719851 -}
	let n1 = 4782 * ((log ((1+ sqrt 5)/2)) / log 10) - (((log 5) / 2)/ log 10) -- +1 Mantissa is the number 
	{- 999.0294106732304 -}
	let n2 = 4781 * ((log ((1+ sqrt 5)/2)) / log 10) - (((log 5) / 2)/ log 10)
	{- 998.8204230329804 -}

	print n
	print n1
	print n2

	putStrLn "Soln: 4782"

	{- Solution: 4782 -}

