{- Reverse a list. -}
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

main = do
	print $ reverse "A man, a plan, a canal, panama!"
	print $ reverse [1,2,3,4]
