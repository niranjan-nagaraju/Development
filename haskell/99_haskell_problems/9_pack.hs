{- Pack consecutive duplicates of list elements into sublists. 
 - If a list contains repeated elements they should be placed in separate sublists. 
 -}

pack [] = []
pack (x:xs) = 
	[[x] ++ (takeWhile (==x) xs)]  ++ pack (dropWhile (==x) xs)

main = do
	print $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
