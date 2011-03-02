{- Rotate a list N places to the left.
 - On -ve N, rotate (len-N) places to the left
 -
 - Trial Run:
 -	 *Main> rotate ['a','b','c','d','e','f','g','h'] 3
 -   "defghabc"
 -  
 -   *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
 -   "ghabcdef"
 -}

rotate list n 
	| (n == 0) = list
	| (n > 0) = rotate' list n
	| otherwise = rotate' list (length list + n)	-- n < 0
	where
	rotate' list n = 
		(drop n list) ++ (take n list)


main = do
	print $ rotate "abcdefgh" 3
	print $ rotate "abcdefgh" (-2)

