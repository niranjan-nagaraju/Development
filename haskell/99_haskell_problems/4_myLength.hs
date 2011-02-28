{- Find the number of elements of a list. -}

myLength lst = 
	myLength' lst 0
	where
	myLength' [] i = i
	myLength' (x:xs) i = myLength' xs (i+1)

main = do
	print $ myLength [123, 456, 789]
	print $ myLength "Hello, world!"
