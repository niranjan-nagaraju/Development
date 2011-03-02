{- Remove the K'th element from a list. -}

removeAt k list  = 
	take (k) list ++ drop k list
