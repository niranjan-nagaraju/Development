{- Eliminate consecutive duplicates of list elements. -}

compress [] = []
compress (x:xs) = 
	[x] ++ compress (dropWhile (==x) xs)

main = do
	print $  compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
