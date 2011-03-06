{- Find the last element of a list. -}

myLast (x:xs) =
	if (null xs)
		then x
		else myLast xs

main = do
	print $ myLast [1,2,3,4]
	print (myLast ['x', 'y', 'z'])
