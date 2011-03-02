{- Duplicate the elements of a list. -}

duplicate [] = []
duplicate (x:xs) = [x] ++ [x] ++ duplicate xs

main = do
	print $ duplicate [1,2,3,4,5]
	print $ duplicate "abcde"
