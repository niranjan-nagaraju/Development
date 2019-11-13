import IO

pairs_array_sum lst sum = 
	[(x,y) | x <- lst, y <- lst, (x+y == sum) && (x<y)]

main = do
	print $ pairs_array_sum [1,2,3,4,5] 5
