{- Find the K'th element of a list -}

elementAt (x:xs) i = 
	if i == 1
		then x
		else elementAt xs (i-1)

main = do
	print $ elementAt [1,2,3] 2
	print $ elementAt "haskell" 5
	print $ elementAt "abcde" 3 
