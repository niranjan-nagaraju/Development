import IO

pair_array_sum lst sum = 
	head $ [(x,y) | x <- lst, y <- lst, (x+y == sum) && (x<y)]

main = do
	print $ pair_array_sum [1,2,3,4,5] 5
