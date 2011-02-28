{- Find the last but one element of a list. -}

myButLast (x:y:xs) = 
	if (xs == [])
		then x
		else myButLast xs


main = do
	print $ myButLast [1,2,3,4]
	print $ myButLast ['a' .. 'z']
