{- Drop every N'th element from a list. -}

dropEvery list 0 = list
dropEvery list n = 
	dropEvery' list n 1
	where
	dropEvery' [] _ _ = []
	dropEvery' (x:xs) n index = 
		if index `mod` n == 0
			then dropEvery' xs n (index+1) 
			else x : dropEvery' xs n (index+1)  	


main = do
	print $ dropEvery "abcdefghik" 3
