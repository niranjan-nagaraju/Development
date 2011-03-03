{- Remove the K'th element from a list. -}

removeAt k list  = 
	take (k-1) list ++ drop k list

main = do
	print $ removeAt 1 "abcd"
	print $ removeAt 2 "abcd"	
	print $ removeAt 3 "abcd"	
	print $ removeAt 4 "abcd"	
