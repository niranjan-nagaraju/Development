{- Split a list into two parts; the length of the first part is given. -}

split list n =
	split' list n []
	where
	split' (x:xs) n left =
		if ( n > 0 )
			then split' xs (n-1) (left ++ [x])	-- Keep collecting to the left until it has 'n' elements
			else [left, x:xs]	-- We've collected enough to the left, Add the remainder to the right and return


main = do
	print $ split [1,2,3,4,5,6] 4
	print $ split "abcdefghik" 3
