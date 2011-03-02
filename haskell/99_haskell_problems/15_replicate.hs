{- Replicate the elements of a list a given number of times. -}

repli [] _ = []
repli (x:xs) times = 
	(map (\y -> x) [1..times]) ++ (repli xs times)

main = do
	print $ repli "abc" 3
