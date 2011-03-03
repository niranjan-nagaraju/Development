{- Insert an element at a given position into a list. -}

insertAt ele list i = 
	(take (i-1) list) ++ [ele] ++ (drop (i-1) list)

main = do
	print $ insertAt 'X' "abcd" 1
	print $ insertAt 'X' "abcd" 2
	print $ insertAt 'X' "abcd" 3
	print $ insertAt 'X' "abcd" 4
	print $ insertAt 'X' "abcd" 5
